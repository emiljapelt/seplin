
type optimize_focus = 
  | Size
  | Speed

type flag_record = {
  mutable opti_focus : optimize_focus
}

let compile_flags : flag_record = {
  opti_focus = Speed
} 