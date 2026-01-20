import gleam/option
import pturso
import parrot/dev

pub fn parrot_to_pturso(p: dev.Param) -> pturso.Param {
  case p {
    dev.ParamInt(x) -> pturso.Int(x)
    dev.ParamString(x) -> pturso.String(x)
    dev.ParamFloat(x) -> pturso.Float(x)
    dev.ParamBool(True) -> pturso.Int(1)
    dev.ParamBool(False) -> pturso.Int(0)
    dev.ParamBitArray(x) -> pturso.Blob(x)
    dev.ParamTimestamp(_) -> panic as "not supported"
    dev.ParamDate(_) -> panic as "not supported"
    dev.ParamList(_) -> panic as "not supported"
    dev.ParamDynamic(_) -> panic as "not supported"
    dev.ParamNullable(option.Some(x)) -> parrot_to_pturso(x)
    dev.ParamNullable(option.None) -> pturso.Null
  }
}

