var to_string = ((any) => {
return String(any)
});
var write = ((s) => {
return process.stdout.write(s)
});
var print = ((any) => {
return write(to_string(any))
});
var println = ((any) => {
var _then_ = print(any);
return print("\n")
});
var floor = ((a) => {
return ~~(a)
});
var round = ((a) => {
return Math.round(a)
});
var ceil = ((a) => {
return Math.ceil(a)
});
var float_to_int = ((a) => {
return ~~(a)
});
var main = (() => {
var a = 34;
var b = 35.69;
return println((a + float_to_int(b)))
});
main()