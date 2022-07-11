module ImageLoaderTests = struct
  open Image_loader
  open Util


  let%test _ =
    print_line @@ Sys.getenv "PWD" ;
    match Image_loader.BMP.image_of_file "tests/red_3x3.bmp"  with
    | Some(image) ->
      let red: Formats.pixel4 = {a=255; r=255; g=0; b=0} in
      Image.get image ~x:0 ~y:0 = red && Image.get image ~x:1 ~y:1 = red
    | None -> false

end
