doPaper = function() {
  
  (function(){
    var canvas = document.getElementById('myCanvas')
    paper.setup(canvas)
    new paper.Tool()     // updates paper.tool and pushes to paper.tools
    }())
  
  var lastRectPos = new paper.Point([0,0])  // Used for creatiung var-sized rects
  var rects = []      // Serialized rect obejcts
                      // tlx,tly,brx,bry, i.e. [[10,10,10,10], ...]
  var dispRects = []  // Displayed objects 
  
  function shouldCreate() {
    return paper.Key.isDown('shift') }
  function shouldCreateDrag() {
    return paper.Key.isDown('command') }
  function shouldRemove () {
    return !shouldCreate() && !shouldCreateDrag() }
  function assureP2(p,p2) {
    return p2 || p.add(new paper.Point([20,20])) }
  function makeRect(p,p2) {
    p2 = assureP2(p,p2)
    var path = new paper.Path.Rectangle(p, p2)
		path.strokeColor = 'white'
		path.fillColor = 'blue'
    path.onMouseDown = (function(){
      if (shouldRemove()) {
        path.remove() }})
        // FIXME remove from rects
    // Hack: save serializable version as part of shape obj
    path._serialized = [p.x,p.y,p2.x,p2.y]        
    dispRects.push(path)
  }
  function clearDispRects() {
    dispRects.forEach( function (r) {
      r && r.remove() })
    dispRects = [] }

  // Load all points from window.rects, populate dispRects with rect objs
  window.load = function () {
    clearDispRects()
    rects.forEach(function(ps) {
      a = ps[0]; b = ps[1]; c = ps[2]; d = ps[3]
      makeRect(new paper.Point([a,b]), new paper.Point([c,d])) })
    paper.view.draw() }
  window.store = function () {
    rects = []
    dispRects.forEach (function (r) {
      rects.push(r._serialized) })
    paper.view.draw() }
  paper.tool.onMouseDown = (function(e){
    if (shouldCreate())
      makeRect(e.point)
    if (shouldCreateDrag())
      lastRectPos = e.point
    if (paper.Key.isDown('escape')) {
      console.log("sending to hs")
      if (window.retFunc) {
        window.store()
        // Send to backend
        window.retFunc(rects) }}})
  paper.tool.onMouseUp = (function(e){
    if (shouldCreateDrag()) {
      makeRect(lastRectPos, e.point) }})
  paper.view.draw() }


window.h = {}
window.h.dummy = function(ret) {}
window.h.start = function(ret) {
  doPaper()
  window.retFunc = ret }
window.h.query = function(ret) {
  store() // Get rects from what we see
  ret(window.rects) }
