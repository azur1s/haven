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
var log = ((s) => {
return console.log(s)
});
var main = (() => {
var o = {foo: 123, bar: "456"};
return log(o)
});
main()