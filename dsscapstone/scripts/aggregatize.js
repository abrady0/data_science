var fs = require('fs');
var path = require('path');

function main(dir, argv) {
  var fn = argv[2];
  var outdir = argv[3];
  if (argv.length !== 4 || !fn || !outdir) {
    console.log('invalid: <infile> <outdir>',fn,outdir); // DONOTCHECKIN
    return;
  }
  if (!fs.existsSync(outdir)) {
    console.error('outdir does not exist',outdir);
    return;
  }
  var p = path.parse(fn);
  var outFn = path.join(outdir, p.name+'_aggregate'+p.ext);  
  console.log('reading from',fn,'writing to',outFn);
  lines = fs.readFileSync(fn, {encoding: 'utf8'}).split('\n');
  console.log('read file\tline count',lines.length); // DONOTCHECKIN
  if (fs.existsSync(outFn)) {
    console.log('removing existing',outFn); // DONOTCHECKIN
    fs.unlinkSync(outFn);
  }
  var agg = {};
  for (var i = 0; i < lines.length; ++i) {
    var line = lines[i];
    if (!agg[line]) {
      agg[line] = 1;
    } else {
      agg[line] += 1;
    }
    if (line % 1000000 === 0) {
      console.log('processed line',i,'of',lines.length,i/lines.length+'%'); // DONOTCHECKIN
    }
  }
  console.log('agg',agg[lines[0]]);
  var lines = [];
  var numKeys = Object.keys(agg).length;
  for (var k in agg) {
    var freq = agg[k];
    lines.push(k+' '+freq);    
  }
  fs.writeFileSync(outFn, lines, {encoding:'utf8'});
}

main(process.cwd(), process.argv);
