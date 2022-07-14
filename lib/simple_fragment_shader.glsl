#version 330 core
out vec4 FragColor;

in vec3 v3_normal;
in vec3 v3_position;
in vec2 v2_tex;

uniform sampler2D u_texture;

void main()
{
  vec4 object_color = vec4(1.0f,0.3f,0.4f, 1.0f);

  vec4 light_color = vec4(1.0f,1.0f,1.0f,1.0f);
  vec3 light_position = vec3(-1.5f,-2.5f, 10.0f);

  vec3 light_direction = normalize(light_position-v3_position);

  float ambient = 0.2f;


  float diffuse = max(dot(v3_normal, light_direction), 0.0f);

  vec3 view_position = vec3(2.0f, 2.0f, 2.0f);
  float specular_strength = 0.8f;
  vec3 view_direction = normalize(view_position - v3_position);
  vec3 reflect_direction = reflect(-light_direction, v3_normal);
  float spec = pow(max(dot(view_direction, reflect_direction), 0.0f), 4);
  float specular = specular_strength * spec;

  vec4 tex_color = texture(u_texture, v2_tex);

   FragColor =  object_color * light_color * (diffuse + ambient + specular);
  //FragColor =  tex_color;
}
