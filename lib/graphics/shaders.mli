type shader_program = { id: int }

val createVertex : source:string -> int
val createFragment : source:string -> int

val createSampleVertex : unit -> int
val createSampleFragment : unit -> int

val createProgram : vertexShader:int -> fragmentShader:int -> int
val createSampleProgram : unit -> shader_program
