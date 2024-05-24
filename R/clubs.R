# TODO: While I'm parsing spreadsheets, I might as well also warn if the meeting
# is coming up without anybody signed up.

# TODO: And I might as well do things like checking that the signed up user will
# be ready/announcing that it's coming. That can also serve to prompt them to
# update the spreadsheet if it's wrong, so it can be right when the meeting is
# posted!

dslc_active_clubs <- function() {
  # TODO: Load info about running clubs. Right now we only use the length, and
  # it isn't super important, so just return a long-enough object.
  return(1:50)
}
