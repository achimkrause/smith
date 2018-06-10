var S = require('../output/Smith')

var button = document.getElementById("button");

button.addEventListener('click', main);


function displayMatrix(mat){
    var smith = S.smith(mat);
}


function main(){
    var str = document.getElementById("area").value;
    var str2 = S.parseAndProcess(str);
    var text = document.getElementById("text");
    text.textContent = str2;
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
}

 