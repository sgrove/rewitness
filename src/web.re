let module Mat4 = {
  type t;
  external create : unit => t = "mat4.create" [@@bs.val];
  external perspective : out::t => fovy::int => aspect::float => near::float => far::float => unit = "mat4.perspective" [@@bs.val];
  external identity : out::t => unit = "mat4.identity" [@@bs.val];
  external translate : out::t => a::t => array float => unit = "mat4.translate" [@@bs.val];
};

let module Gl = {
  type context;
  type program;
  type shader;
  type float32array;
  external setViewportWidth : context => int => unit = "viewportWidth" [@@bs.set];
  external setViewportHeight : context => int => unit = "viewportHeight" [@@bs.set];
  external setClearColor : context => float => float => float => float => unit = "clearColor" [@@bs.send];
  external createProgram : context => program = "createProgram" [@@bs.send];
  external createShader : context => int => shader = "createShader" [@@bs.send];
  external attachShader : context => program => shader => unit = "createProgram" [@@bs.send];
  external shaderSource : context => shader => string => unit = "shaderSource" [@@bs.send];
  external compileShader : context => shader => unit = "compileShader" [@@bs.send];
  external attachShader : context => program => shader::shader => unit = "attachShader" [@@bs.send];
  external linkProgram : context => program => unit = "linkProgram" [@@bs.send];
  external useProgram : context => program => unit = "useProgram" [@@bs.send];
  type buffer;
  type attribute;
  type uniform;
  external createBuffer : context => buffer = "createBuffer" [@@bs.send];
  external bindBuffer : context => int => buffer => unit = "bindBuffer" [@@bs.send];
  external bufferData : context => int => float32array => int => unit = "bufferData" [@@bs.send];
  external setViewport : context => int => int => int => int => unit = "viewport" [@@bs.send];
  external setClearBit : context => int => unit = "clear" [@@bs.send];
  external createFloat32Array : array float => float32array = "Float32Array" [@@bs.new];
  external getUniformLocation : context => program => string => uniform = "getUniformLocation" [@@bs.send];
  external getAttribLocation : context => program => string => attribute = "getAttribLocation" [@@bs.send];
  external enableVertexAttribArray : context => attribute => unit = "enableVertexAttribArray" [@@bs.send];
  external vertexAttribPointer : context =>
                                 attribute =>
                                 int =>
                                 int =>
                                 Js.boolean =>
                                 int =>
                                 int =>
                                 unit = "vertexAttribPointer" [@@bs.send];
  external setUniformMatrix4fv : context => uniform => Js.boolean => Mat4.t => unit = "uniformMatrix4fv" [@@bs.send];
  /* Can return other value types as well, see https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Types */
  external getProgramParameter : context => program => int => Js.boolean = "getProgramParameter" [@@bs.send];
  external getShaderParameter : context => shader => int => Js.boolean = "getShaderParameter" [@@bs.send];
  external getShaderInfoLog : context => shader => string = "getShaderInfoLog" [@@bs.send];
  external getShaderSource : context => shader => string = "getShaderSource" [@@bs.send];
  external drawArrays : context => int => int => int => unit = "drawArrays" [@@bs.send];
};

/* Dumb camera, not actually a camera like you think */
type gl_camera = {projection_matrix: Mat4.t, model_view_matrix: Mat4.t};

let module Document = {
  type element;
  type window;
  let window: window = [%bs.raw "window"];
  external setGlDebug : window => Gl.context => unit = "debugContext" [@@bs.set];
  external getElementById : string => element = "document.getElementById" [@@bs.val];
  external getContext : element => string => Gl.context = "getContext" [@@bs.send];
  external getWidth : element => int = "width" [@@bs.get];
  external getHeight : element => int = "height" [@@bs.get];
  external requestAnimationFrame : (unit => unit) => unit = "window.requestAnimationFrame" [@@bs.val];
  /* external getCurrentMilliseconds : unit => int = "(new Date()).getTime()" [@@bs.val]; */
};

type gl_env = {camera: gl_camera, canvas: Document.element, gl: Gl.context};

let set_projection (canvas: Document.element) (camera: gl_camera) => {
  let canvas_width = Document.getWidth canvas;
  let canvas_height = Document.getHeight canvas;
  Mat4.perspective
    camera.projection_matrix
    45
    (float_of_int canvas_width /. float_of_int canvas_height)
    0.1
    100.0;
  ()
};

let translate_camera (camera: gl_camera) (offset: array float) =>
  Mat4.translate camera.model_view_matrix camera.model_view_matrix offset;

let reset_mv (camera: gl_camera) => Mat4.identity camera.model_view_matrix;

let build_gl_env (canvas: Document.element) :gl_env => {
  let gl = Document.getContext canvas "webgl";
  let gl_camera = {projection_matrix: Mat4.create (), model_view_matrix: Mat4.create ()};
  let env = {camera: gl_camera, canvas, gl};
  let canvas_width = Document.getWidth canvas;
  let canvas_height = Document.getHeight canvas;
  Gl.setViewportWidth gl canvas_width;
  Gl.setViewportHeight gl canvas_height;
  Gl.setViewport gl 0 0 canvas_width canvas_height;
  Gl.setClearColor gl 0.0 0.0 0.0 1.0;
  Gl.setClearBit gl (Constants.color_buffer_bit lor Constants.depth_buffer_bit);
  env
};

let add_program (env: gl_env) vertex_shader_source fragment_shader_source :Gl.program => {
  let vertex_shader = Gl.createShader env.gl Constants.vertex_shader;
  Gl.shaderSource env.gl vertex_shader vertex_shader_source;
  Gl.compileShader env.gl vertex_shader;
  let fragment_shader = Gl.createShader env.gl Constants.fragment_shader;
  Gl.shaderSource env.gl fragment_shader fragment_shader_source;
  Gl.compileShader env.gl fragment_shader;
  let program = Gl.createProgram env.gl;
  Gl.attachShader env.gl program vertex_shader;
  Gl.attachShader env.gl program fragment_shader;
  Js.log (Gl.getShaderSource env.gl vertex_shader);
  Js.log (Gl.getShaderSource env.gl fragment_shader);
  Js.log fragment_shader;
  Js.log vertex_shader;
  Gl.linkProgram env.gl program;
  if (Js.false_ == Gl.getProgramParameter env.gl program Constants.link_status) {
    Js.log "Failed to initialize shader: ";
    Js.log (Gl.getShaderInfoLog env.gl vertex_shader);
    Js.log (Gl.getShaderParameter env.gl vertex_shader Constants.compile_status);
    Js.log (Gl.getShaderInfoLog env.gl fragment_shader);
    Js.log (Gl.getShaderParameter env.gl fragment_shader Constants.compile_status)
  } else {
    Js.log "Shader initialized!"
  };
  program
};

let vertex_shader_source = {|
  attribute vec3 aVertexPosition;
  attribute vec4 aVertexColor;

  uniform mat4 uMVMatrix;
  uniform mat4 uPMatrix;

  varying vec4 vColor;

  void main(void) {
    gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
    vColor = aVertexColor;
  }|};

let fragment_shader_source = {|
  precision mediump float;

  varying vec4 vColor;

  void main(void) {
    gl_FragColor = vColor;
  }
|};

let env = build_gl_env (Document.getElementById "main");

let program = add_program env vertex_shader_source fragment_shader_source;

Gl.useProgram env.gl program;

let vertex_position_attrib = Gl.getAttribLocation env.gl program "aVertexPosition";

let vertex_color_attrib = Gl.getAttribLocation env.gl program "aVertexColor";

Gl.enableVertexAttribArray env.gl vertex_position_attrib;

Gl.enableVertexAttribArray env.gl vertex_color_attrib;

let p_matrix_uniform = Gl.getUniformLocation env.gl program "uPMatrix";

let mv_matrix_uniform = Gl.getUniformLocation env.gl program "uMVMatrix";

Gl.setUniformMatrix4fv env.gl p_matrix_uniform Js.false_ env.camera.projection_matrix;

Gl.setUniformMatrix4fv env.gl mv_matrix_uniform Js.false_ env.camera.model_view_matrix;

let triangle_vertex_buffer = Gl.createBuffer env.gl;

let triangle_color_buffer = Gl.createBuffer env.gl;

let square_vertex_buffer = Gl.createBuffer env.gl;

let square_color_buffer = Gl.createBuffer env.gl;

let triangle_vertices = [|0.0, 1.0, 0.0, (-1.0), (-1.0), 0.0, 1.0, (-1.0), 0.0|];

let triangle_colors = [|1.0, 0.0, 0.0, 1.0, (-0.0), 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0|];

let square_vertices = [|1.0, 1.0, 0.0, (-1.0), 1.0, 0.0, 1.0, (-1.0), 0.0, (-1.0), (-1.0), 0.0|];

let square_colors = [|
  0.5,
  0.5,
  1.0,
  1.0,
  0.5,
  0.5,
  1.0,
  1.0,
  0.5,
  0.5,
  1.0,
  1.0,
  0.5,
  0.5,
  1.0,
  1.0
|];

set_projection env.canvas env.camera;

Gl.bindBuffer env.gl Constants.array_buffer triangle_vertex_buffer;

Gl.bufferData
  env.gl Constants.array_buffer (Gl.createFloat32Array triangle_vertices) Constants.static_draw;

Gl.bindBuffer env.gl Constants.array_buffer triangle_color_buffer;

Gl.bufferData
  env.gl Constants.array_buffer (Gl.createFloat32Array triangle_colors) Constants.static_draw;

Gl.bindBuffer env.gl Constants.array_buffer square_vertex_buffer;

Gl.bufferData
  env.gl Constants.array_buffer (Gl.createFloat32Array square_vertices) Constants.static_draw;

Gl.bindBuffer env.gl Constants.array_buffer square_color_buffer;

Gl.bufferData
  env.gl Constants.array_buffer (Gl.createFloat32Array square_colors) Constants.static_draw;

reset_mv env.camera;

/* Move camera, draw triangle */
translate_camera env.camera [|(-1.5), 0.0, (-7.0)|];

Gl.bindBuffer env.gl Constants.array_buffer triangle_vertex_buffer;

Gl.vertexAttribPointer env.gl vertex_position_attrib 3 Constants.float_ Js.false_ 0 0;

Gl.bindBuffer env.gl Constants.array_buffer triangle_color_buffer;

Gl.vertexAttribPointer env.gl vertex_color_attrib 4 Constants.float_ Js.false_ 0 0;

Gl.setUniformMatrix4fv env.gl p_matrix_uniform Js.false_ env.camera.projection_matrix;

Gl.setUniformMatrix4fv env.gl mv_matrix_uniform Js.false_ env.camera.model_view_matrix;

Js.log "Draw triangle";

Gl.drawArrays env.gl Constants.triangles 0 3;

/* Move camera, draw square */
translate_camera env.camera [|3.0, 0.0, 0.0|];

Gl.bindBuffer env.gl Constants.array_buffer square_vertex_buffer;

Gl.vertexAttribPointer env.gl vertex_position_attrib 3 Constants.float_ Js.false_ 0 0;

Gl.bindBuffer env.gl Constants.array_buffer square_color_buffer;

Gl.vertexAttribPointer env.gl vertex_color_attrib 4 Constants.float_ Js.false_ 0 0;

Gl.setUniformMatrix4fv env.gl p_matrix_uniform Js.false_ env.camera.projection_matrix;

Gl.setUniformMatrix4fv env.gl mv_matrix_uniform Js.false_ env.camera.model_view_matrix;

Js.log "Draw square";

Gl.drawArrays env.gl Constants.triangle_strip 0 4;

Js.log "Finished drawing!";

Document.setGlDebug Document.window env.gl;

Js.log env;

let rec tick () => {
  Js.log "Tick!";
  Document.requestAnimationFrame tick
};

/* Js.log (Document.getCurrentMilliseconds ()) */
Document.requestAnimationFrame tick;
