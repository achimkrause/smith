var S = require('../output/Smith')


module.exports = {main : main}

function main(){
    var str = document.getElementById("area").value;
    var str2 = S.parseAndProcess(str);
    var text = document.getElementById("text");
    text.textContent = str2;
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
}

 