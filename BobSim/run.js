var printfn = function(){
    var felt = document.getElementById("out1");
    return function(str,c){
        if(c !== undefined){
            felt.innerHTML += "<font color='"+c+"'>"+str+"</font><br>";
            }
        else {
            felt.innerHTML += str + "<br>";
            }
        };
    }();
var printPrg = function(prg){
    var lines = prg.split("\n");
    var cb = document.createElement("table");
    for(var i = 1; i < lines.length; i++){
        var line = document.createElement("tr");
        var nr = document.createElement("td");
        var cline = document.createElement("td");
        nr.style.borderRight = "2px dotted red";
        nr.innerHTML = (i + 1).toString();
        cline.style.whiteSpace = "pre";
        cline.innerHTML = lines[i];
        line.appendChild(nr);
        line.appendChild(cline);
        cb.appendChild(line);
        }
    cb.style.width = "100%";
    cb.style.backgroundColor = "black";
    cb.style.color = "white";
    cb.style.fontFamily = "monospace";
    cb.style.fontSize = "12pt";
    document.body.appendChild(cb);
    };
var prgs = document.getElementsByClassName("bobprg");
var machine = new Machine();
var bob = new BobSim(machine);
var prg = prgs[6].innerHTML;
printPrg(prg);
bob.exec(prg);
        
