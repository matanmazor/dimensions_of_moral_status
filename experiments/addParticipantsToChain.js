const max_N = 15;
var counter = 0;

setInterval(function(){
 console.log('looping')
 var numFinished =  (document.body.innerText.match(/AWAITING/g) || []).length;
 if (numFinished> counter & numFinished <= max_N) {
   console.log('new participant!')
   counter= numFinished;
   document.querySelectorAll('[data-testid="increase-places"]')[0].click();
   setTimeout(()=>document.getElementsByClassName('el-button button el-button--primary el-button--large')[0].click(),
3000)
   
}
}, 5000)

