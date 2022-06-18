open struct
  module Gl = Tgl4.Gl
end

let sampleTriangle = [|
  -0.5; -0.5; 0.0;
  0.5; -0.5; 0.0;
  0.0;  0.5; 0.0
|]
;;

 

let vertexShaderSource =  "#version 330 core\n"
    "layout (location = 0) in vec3 aPos;\n"
    "void main()\n"
    "{\n"
    "   gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n"
    "}\0"

let fragmentShaderSource = "#version 330 core
out vec4 FragColor;

void main()
{
    FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
} "
