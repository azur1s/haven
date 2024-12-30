let print = ((s) => {
return process.stdout.write(s)
});
let println = ((s) => {
let _then_ = print(s);
return print("\n")
});
let main = ((_unit_) => {
return print("Hello, World\n")
});
main()