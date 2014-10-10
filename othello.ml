type config = {
    nbcols  : int;
    nbrows  : int
} ;;
type coord = {
    x   : int;
    y   : int
} ;;
type cell = White | Black | Empty
type board = cell array array

let default_config = { nbcols=8; nbrows=10} ;;
