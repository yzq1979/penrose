var CANVAS_WIDTH    = 800
var CANVAS_HEIGHT   = 700
var DEBUG           = true
// var DEBUG           = false

function main() {
    console.log("started penrose geometry test module")
    var s = Snap("#svgdiv");
    $("#svgdiv").css("width",  CANVAS_WIDTH);
    $("#svgdiv").css("height", CANVAS_HEIGHT);

    //testIntersect(s)
    // var l1 = [ 10, 10, 100, 100 ]
    // var l2 = [ 50, 150, 250, 50 ]
    var l1 = genLine([0, CANVAS_WIDTH], [0, CANVAS_HEIGHT])
    var l2 = genLine([0, CANVAS_WIDTH], [0, CANVAS_HEIGHT])
    // var l2 = JSON.parse(JSON.stringify(l1))
    // l2.pt1.y += 100
    // l2.pt2.y += 100
    // renderPolylines(s, [toList([l1.pt1, l1.pt2]), toList([l2.pt1, l2.pt2])])
    // //console.log(l1pts)
    //shortestAbsDistance(l1, l2, s)

    var poly1 = randomPolygon(300);
    var poly2 = randomPolygon(110);
    drawPolygon(poly1, s)
    drawPolygon(poly2, s)
    checkAllPointContainment(poly1, poly2, s)
    checkAllPointContainment(poly2, poly1, s)

    absMinDist(poly1, poly2, s)
    minDist(poly1, poly2, s)
}

function genPoint([xmin, xmax], [ymin, ymax]) {
    x = Math.floor(Math.random() * (xmax - xmin)) + xmin;
    y = Math.floor(Math.random() * (ymax - ymin)) + ymin;
    return {x, y}
}
function genLine(xRange, yRange) {
    pt1 = genPoint(xRange, yRange)
    pt2 = genPoint(xRange, yRange)
    return {pt1, pt2}
}

/****************************************
* List of points to list of coordinate objects
*****************************************/
function toCoords(list) {
    var ptList = []
    for(var i = 0; i < list.length; i+=2){
        var pts = {}
        pts.x = list[i]
        pts.y = list[i+1]
        ptList.push(pts)
    }
    return ptList
}


/****************************************
* Render intersection point
*****************************************/
function renderIntersect(s, l1, l2) {
    renderPolylines(s, [l1, l2])
    p = intersect(l1, l2)
    if(p) {
        if (p === true) {
            console.log("The lines are colinear!")
        } else {
            console.log("Intersection found at: ", p)
            renderPoint(s, p)
        }
    }
    else {
        console.log("The lines do not intersect!")
    }
}

/****************************************
* https://www.paulirish.com/2009/random-hex-color-code-snippets/
*****************************************/
function randomColor() {
    return '#' + Math.floor(Math.random()*16777215).toString(16);
}

/****************************************
*
*****************************************/
function renderPolylines(s, ptss) {
    var res = []
    ptss.forEach(function(pts) {
        res += renderPolyline(s, pts)
    })
    return res
}

/****************************************
*
*****************************************/
function renderPoint(s, pt) {
    [x, y] = pt
    var renderedPoint = s.circle(x, y, 5)
    renderedPoint.attr({ fill: "red" })
    return renderedPoint
}

/****************************************
*
*****************************************/
function renderPolyline(s, pts) {
    if(DEBUG) console.log("rendering", pts);
    [x1, y1, x2, y2] = pts
    var renderedLine = s.polyline(pts)
    var color = randomColor()
    renderedLine.attr({
        "stroke-width": 5,
        stroke: color
    })
    renderedLine.drag()
    var endPoint1 = s.circle(x1, y1, 5)
    var endPoint2 = s.circle(x2, y2, 5)
    var group = s.g(renderedLine, endPoint1, endPoint2)
    group.attr({ fill: color })
    return group
}

/****************************************
* Renders a polygon where the pts is a list of points, not coord objects
*****************************************/
function renderPolygonPointList(s, pts) {
    if(DEBUG) console.log("rendering polygon", pts);
    var polygon = s.polygon(pts)
    var color = randomColor()
    polygon.attr({
        "stroke-width": 5,
        fill: color
    })
    polygon.drag()
    return polygon
}

// line intercept math by Paul Bourke http://paulbourke.net/geometry/pointlineplane/
// Determine the intersection point of two line segments
// Return FALSE if the lines don't intersect
function intersect([x1, y1, x2, y2], [x3, y3, x4, y4]) {

  // Check if none of the lines are of length 0
	if ((x1 === x2 && y1 === y2) || (x3 === x4 && y3 === y4)) {
		return false
	}

	denominator = ((y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1))

  // Lines are parallel
	if (denominator === 0) {

        // lines are colinear
        numeratorA  = ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3))
        numeratorB  = ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3))
        if(numeratorA == 0 && numeratorB == 0)
            return true

		return false
	}

	let ua = ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / denominator
	let ub = ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)) / denominator

  // is the ion along the segments
	if (ua < 0 || ua > 1 || ub < 0 || ub > 1) {
		return false
	}

  // Return a object with the x and y coordinates of the intersection
	let x = x1 + ua * (x2 - x1)
	let y = y1 + ua * (y2 - y1)

	return [x, y]
}

/****************************************
* Based on https://github.com/substack/point-in-polygon/
*****************************************/
function inPolygon(point, vs) {
    // ray-casting algorithm based on
    // http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html

    var x = point.x;
    var y = point.y;

    var inside = false;
    for (var i = 0, j = vs.length - 1; i < vs.length; j = i++) {
        var xi = vs[i].x, yi = vs[i].y;
        var xj = vs[j].x, yj = vs[j].y;

        var intersect = ((yi > y) != (yj > y))
            && (x < (xj - xi) * (y - yi) / (yj - yi) + xi);
        if (intersect) inside = !inside;
    }

    return inside;
};
//https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
function sqr(x) { return x * x }
function dist2(v, w) { return sqr(v.x - w.x) + sqr(v.y - w.y) }


/****************************************
* finds dist of point to line, returns line in coord form and distance
*****************************************/
function distToSegment(pt, l, s) {
    [v, w] = [l.pt1, l.pt2]
    var l2 = dist2(v, w);
    if (l2 == 0) return dist2(pt, v);
    var t = ((pt.x - v.x) * (w.x - v.x) + (pt.y - v.y) * (w.y - v.y)) / l2;
    t = Math.max(0, Math.min(1, t));
    var pt1 = { x: pt.x, y: pt.y }
    var pt2 = { x: v.x + t * (w.x - v.x), y: v.y + t * (w.y - v.y) }
    var line = { pt1, pt2 }
    dist = Math.sqrt(dist2(pt, { x: v.x + t * (w.x - v.x),
        y: v.y + t * (w.y - v.y) }))


    // var sign = inPolygon(pt, polygon)
    // if(sign){
    //   dist = dist*(-1)
    //   console.log(sign + " is it negative? dist is: " + dist)
    // }
    return {line, dist}
}


// function distToSegment(p, v, w, s) {
//     return Math.sqrt(distToSegmentSquared(p, v, w, s));
// }

/****************************************
* absolute minimum distance between two polygons
*****************************************/
function absMinDist(p1, p2, s){
  min = {}
  min.dist = 100000
  for(var i = 0; i < p1.length-1; i++){
    for(var j = 0; j < p2.length-1; j++){
      var a = {}
      var b = {}
      a.pt1 = p1[i]
      a.pt2 = p1[i+1]
      b.pt1 = p2[j]
      b.pt2 = p2[j+1]

      temp = shortestAbsDistance(a, b, p1, p2, s)
      if (temp.dist < min.dist){
        min.dist = temp.dist
        min.line = temp.line
      }
    }
  }
  // for(var i = 0; i < p2.length-1; i++){
  //   for(var j = 0; j < p1.length-1; j++){
  //     var a = {}
  //     var b = {}
  //     a.pt1 = p2[i]
  //     a.pt2 = p2[i+1]
  //     b.pt1 = p1[j]
  //     b.pt2 = p1[j+1]
  //
  //     temp = shortestAbsDistance(a, b, p1, p2, s)
  //     if (temp.dist < min.dist){
  //       min.dist = temp.dist
  //       min.line = temp.line
  //     }
  //   }
  // }
  //var m = min.line
  console.log(min)
  if(min.dist != 0){
    var l = s.polyline(min.line.pt1.x, min.line.pt1.y, min.line.pt2.x, min.line.pt2.y)
    l.attr({
      stroke: "blue",
  		strokeWidth: 2
    })
  }
  else{
    var c = s.circle(min.line.pt1.x, min.line.pt1.y, 5);
    c.attr({
      fill: "blue"
    })
  }
}

/****************************************
* most negative/minimum distance between polygons
*****************************************/
function minDist(p1, p2, s){
    min = {}
    min.dist = Number.MAX_SAFE_INTEGER
    for(var i = 0; i < p1.length-1; i++){
      for(var j = 0; j < p2.length-1; j++){
        var a = {}
        var b = {}
        a.pt1 = p1[i]
        a.pt2 = p1[i+1]
        b.pt1 = p2[j]
        b.pt2 = p2[j+1]

        temp = shortestDistance(a, b, p1, p2, s)
        if (temp.dist < min.dist){
          min.dist = temp.dist
          min.line = temp.line
        }
      }
    }
    //var m = min.line
    console.log(min)
    if(min.dist != 0){
      var l = s.polyline(min.line.pt1.x, min.line.pt1.y, min.line.pt2.x, min.line.pt2.y)
      l.attr({
        stroke: "grey",
    		strokeWidth: 1
      })
    }
    else{
      var c = s.circle(min.line.pt1.x, min.line.pt1.y, 5);
      c.attr({
        fill: "grey"
      })
    }
}

/****************************************
* Finds shortest distance, returns
*****************************************/
function shortestAbsDistance(l1, l2, poly1, poly2, s){
    var ret = {}
    dists = []
    var inter = intersect(toList([l1.pt1, l1.pt2]), toList([l2.pt1, l2.pt2]))
    if(inter){ //T
      var l1 = { pt1: { x:inter[0], y:inter[1] } }
      ret = {line: l1, dist: 0}
      return ret
    }
    else{
      dists[0] = distToSegment(l1.pt1, l2, s)
      dists[1] = distToSegment(l1.pt2, l2, s)
      dists[2] = distToSegment(l2.pt1, l1, s)
      dists[3] = distToSegment(l2.pt2, l1, s)
      var minimum = Number.MAX_SAFE_INTEGER
      for(var i = 0; i < 4; i++){
          if(dists[i].dist < minimum){
              minimum = dists[i].dist
              ret = dists[i]
          }
        }
    }
    var line = [ret.line.pt1.x, ret.line.pt1.y, ret.line.pt2.x, ret.line.pt2.y]
    // var shortestLine = s.polyline(line)
    // shortestLine.attr({
    //     "stroke-width": 2,
    //     stroke: "red"
    // })
    // console.log("SHORTEST DISTANCE " + dists[index].dist);

    return ret
}

/****************************************
* Finds shortest distance, returns
*****************************************/
function shortestDistance(l1, l2, poly1, poly2, s){
    dists = []

    dists[0] = distToSegment(l1.pt1, l2, s)
    var sign = inPolygon(l1.pt1, poly2)
    if(sign){
      dists[0].dist *= (-1)
      console.log(dists[0].dist + " is inside poly2, is now negative");
    }

    dists[1] = distToSegment(l1.pt2, l2, s)
    var sign = inPolygon(l1.pt2, poly2)
    if(sign){ dists[1].dist *= (-1)}

    dists[2] = distToSegment(l2.pt1, l1, s)
    var sign = inPolygon(l2.pt1, poly1)
    if(sign){ dists[2].dist *= (-1)}

    dists[3] = distToSegment(l2.pt2, l1, s)
    var sign = inPolygon(l2.pt2, poly1)
    if(sign){ dists[3].dist *= (-1)}

    var minimum = Number.MAX_SAFE_INTEGER
    var index = -1
    for(var i = 0; i < 4; i++){
        if(Math.abs(dists[i].dist) < minimum){
            minimum = dists[i].dist
            index = i
        }
    }
    //console.log(dists[index])
    // var l = dists[index].line
    // var shortestLine = s.polyline(l.pt1.x, l.pt1.y, l.pt2.x, l.pt2.y)
    // shortestLine.attr({
    //     "stroke-width": 5,
    //     stroke: "red"
    // })
  //  if(DEBUG) console.log("SHORTEST DISTANCE " + dists[index].dist);

    return dists[index]
}


/****************************************
* Given coordinates, make a flat list
*****************************************/
function toList(coords) {
    lst = []
    coords.forEach(function(pt) {
      lst.push(pt.x)
      lst.push(pt.y)
    });
    return lst
}


/****************************************
* Given an array of Coordinate objects, draw a polygon
*****************************************/
function drawPolygon(polygon, s){
  pts = []
  count = 0
  for(var i = 0; i < polygon.length; i++){
    pts[count] = polygon[i].x
    pts[count+1] = polygon[i].y
    count += 2
  }
  // color = ["red", "blue", "green", "yellow"]
  // color[Math.floor(Math.random() * 4)]
  var randColor = Math.floor(Math.random() * 999)
  var colorStr = "#" + randColor
  // if(DEBUG){
  //   console.log(randColor)
  //   console.log("Array of points to send to snap polyline function: ")
  //   console.log(pts)
  // }
  var p = s.polyline(pts)
  p.attr({
    fill: colorStr,
    "fill-opacity": 0.5,
		stroke: "black",
		strokeWidth: 2
  })
  p.drag()
}
// /*************************************
// * Checks that one point pt is inside a poylgon p
// *************************************/
// function containedPoint(pt, p){
//   var ray =
//   for(var i = 0; i < p.length; i++){
//
//   }
// }


/*************************************
* Checks whether points in p1 are inside p2 (if visualizepoints
* flag, then shows green for contained points, red for noncontained)
**************************************/
function checkAllPointContainment(p1, p2, s){
  var inside = false;
  var visualizePoints = true

  for(var i = 0; i < p1.length; i++){
    if (inPolygon(p1[i], p2)){
      inside = true;
      fillColor = "#0C0"
    } else fillColor = "#F00"
    if (visualizePoints){
      var point = s.circle(p1[i].x, p1[i].y, 4)
      point.attr({
        fill: fillColor
      })
    }
  }
  return inside;
}


/*************************************
* Creates random polygon points
**************************************/
function randomPolygon(rmaxnew){
  var xCenter = Math.floor(Math.random() * 300) + 200;
  var yCenter = Math.floor(Math.random() * 300) + 200;
  var rmax = rmaxnew, rmin =40;
  var pointmax = 16, pointmin = 3;
  var numPoints = Math.floor(Math.random() * (pointmax-pointmin)) + pointmin;

  if(DEBUG){
    console.log("Center: " + xCenter + " " + yCenter);
    console.log("numPoints is " + numPoints);
  }

  poly = []
  for(var i = 0; i < numPoints; i++){
    var pt = {}
    var r = Math.floor(Math.random() * (rmax-rmin)) + rmin;
    // console.log("R at " + i + " is " + r);
    pt.x = (xCenter + r * Math.cos(2 * Math.PI * i / numPoints))
    pt.y = (yCenter + r * Math.sin(2 * Math.PI * i / numPoints));
    poly.push(pt)
  }
  var end = {}
  end.x = poly[0].x
  end.y = poly[0].y
  poly.push(end)

  return poly
}

/*************************************
* Draws a polygon from an SVG by taking all points along the hull of a polygon
**************************************/
function drawHullPointsFromSVG(inputSVG, s){
  newpoints = []
  count = 0;
  parsedSVG = parsePathString(inputSVG)
  for (var i in parsedSVG) {
  	if (parsedSVG[i].code === 'Q' || parsedSVG[i].code === 'M' || parsedSVG[i].code === 'C'	|| parsedSVG[i].code === 'L' || parsedSVG[i].code === 'T'){
  		newpoints[count] = parsedSVG[i].x
  		newpoints[count+1] = parsedSVG[i].y
  		count+=2
  	}
  	else if (parsedSVG[i].code === 'H'){
  		newpoints[count] = parsedSVG[i].x
  		newpoints[count+1] = newpoints[count-1]
  		count+=2
  	}
  	else if (parsedSVG[i].code === 'V'){
  		newpoints[count] = newpoints[count-1]
  		newpoints[count+1] = parsedSVG[i].y
  		count+=2
  	}
  	else if (parsedSVG[i].code === 'Z'){ //TWO NEGATIVE numbers means there is a break in the polgon
  		newpoints[count] = newpoints[0]    //similar to "pick up pen, move, put down pen"
  		newpoints[count+1] = newpoints[1]
  		newpoints[count+2] = -1           //negative numbers are placeholders for "pen up"
  		newpoints[count+3] = -1
  		count+=4
  	}
  	//TODO add in A elliptical
  	else
  		count = count
  }
  return newpoints
}




// Main function invokation
$(document).ready(function () {
    main();
});
