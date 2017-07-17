// need to define these, otherwise doo test runner fails with 'print is not defined' error
var Module = {
    'print': function(text) {  console.log(text); },
    'printErr': function(text) { console.error(text); }
};
