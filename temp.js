let print = ((a) => {
return console.log(a)
});
let foo = (() => {
let x = 34;
let y = 35;
return (x + y)
})();
let id = ((x) => {
return x
});
let main = ((_unit_) => {
let fib = ((n) => ((n <= 1) ? n : (fib((n - 1)) + fib((n - 2)))));
let result = id(fib(10));
let _then_ = print(result);
return print(id(foo))
});
main()