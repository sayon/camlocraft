#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <inttypes.h>

void set_raw_float(void* ba, size_t offset, float value) {
  const char* p = ba;
  float* pf = (float*) (p + offset);
  *pf = value;
}
CAMLprim value caml_stub_set_raw_float(value bigarray, value offset, value fval )
{
  double d = Double_val( fval );
  set_raw_float( Caml_ba_data_val(bigarray), Int_val(offset), (float)d);
  return Val_unit;
}


void set_raw_int(void* ba, size_t offset, int64_t value) {
  const char* p = ba;
  int64_t* pf = (int64_t*) (p + offset);
  *pf = value;
}

CAMLprim value caml_stub_set_raw_int(value bigarray, value offset, value ival )
{
  set_raw_int( Caml_ba_data_val(bigarray), Int_val(offset), Int_val( ival ) );
  return Val_unit;
}

float get_raw_float(void* ba, size_t offset) {
  const char* p = ba;
  return *( (float*) (p + offset) );
}
