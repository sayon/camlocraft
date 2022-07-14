
let%test _ =
  match Bmp.image_of_file "tests/red_3x3.bmp"  with
  | Some(image) ->
    let red: Image.Formats.pixel4 = {a=255; r=255; g=0; b=0} in
    Image.Image.get image ~x:0 ~y:0 = red && Image.Image.get image ~x:1 ~y:1 = red
  | None -> false
