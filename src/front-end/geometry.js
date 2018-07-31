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

    renderPolylines(s, [toList([l1.pt1, l1.pt2]), toList([l2.pt1, l2.pt2])])
    //console.log(l1pts)
    shortestDistance(l1, l2, s)
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

function shortestDistance(l1, l2, s){
    dists = []
    dists[0] = distToSegment(l1.pt1, l2, s)
    dists[1] = distToSegment(l1.pt2, l2, s)
    dists[2] = distToSegment(l2.pt1, l1, s)
    dists[3] = distToSegment(l2.pt2, l1, s)
    var minimum = Number.MAX_SAFE_INTEGER
    var index = -1
    for(var i = 0; i < 4; i++){
        if(dists[i].dist < minimum){
            minimum = dists[i].dist
            index = i
        }
    }
    console.log(dists[index])
    var l = dists[index].line
    var shortestLine = s.polyline(l.pt1.x, l.pt1.y, l.pt2.x, l.pt2.y)
    shortestLine.attr({
        "stroke-width": 5,
        stroke: "red"
    })
}

function toList(points) {
    lst = []
    points.forEach(function(pt) {
        lst.push(pt.x)
        lst.push(pt.y)
    })
    return lst
}

function toPoints(list) {
    var ptList = []
    for(var i = 0; i < list.length; i+=2){
        var pts = {}
        pts.x = list[i]
        pts.y = list[i+1]
        ptList.push(pts)
    }
    return ptList
}

function testIntersect(s) {
    // normal intersect 1
    // var l1 = [ 10, 10, 100, 100 ]
    // var l2 = [ 50, 150, 250, 50 ]

    // normal non-intersect 1
    // var l1 = [ 10, 10, 100, 100 ]
    // var l2 = [ 50, 150, 250, 50 ]

    // parallel
    // var l1 = [ 200, 400, 400, 400 ]
    // var l2 = [ 200, 300, 400, 300 ]

    // point intersection
    // var l1 = [ 100, 400, 500, 400 ]
    // var l2 = [ 300, 200, 300, 400 ]

    // colinear
    var l1 = [ 100, 400, 500, 400 ]
    var l2 = [ 200, 400, 400, 400 ]
    renderIntersect(s, l1, l2)
    renderPolygon(s, [100, 400, 500, 400, 100, 400])
}

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

// https://www.paulirish.com/2009/random-hex-color-code-snippets/
function randomColor() {
    return '#' + Math.floor(Math.random()*16777215).toString(16);
}

function renderPolylines(s, ptss) {
    var res = []
    ptss.forEach(function(pts) {
        res += renderPolyline(s, pts)
    })
    return res
}

function renderPoint(s, pt) {
    [x, y] = pt
    var renderedPoint = s.circle(x, y, 5)
    renderedPoint.attr({ fill: "red" })
    return renderedPoint
}

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

function renderPolygon(s, pts) {
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

  // is the intersection along the segments
	if (ua < 0 || ua > 1 || ub < 0 || ub > 1) {
		return false
	}

  // Return a object with the x and y coordinates of the intersection
	let x = x1 + ua * (x2 - x1)
	let y = y1 + ua * (y2 - y1)

	return [x, y]
}

// https://github.com/substack/point-in-polygon/
function inPolygon(point, vs) {
    // ray-casting algorithm based on
    // http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html

    var x = point[0], y = point[1];

    var inside = false;
    for (var i = 0, j = vs.length - 1; i < vs.length; j = i++) {
        var xi = vs[i][0], yi = vs[i][1];
        var xj = vs[j][0], yj = vs[j][1];

        var intersect = ((yi > y) != (yj > y))
            && (x < (xj - xi) * (y - yi) / (yj - yi) + xi);
        if (intersect) inside = !inside;
    }

    return inside;
};
//https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
function sqr(x) { return x * x }
function dist2(v, w) { return sqr(v.x - w.x) + sqr(v.y - w.y) }

function distToSegment(p, l, s) {
    [v, w] = [l.pt1, l.pt2]
    var l2 = dist2(v, w);
    if (l2 == 0) return dist2(p, v);
    var t = ((p.x - v.x) * (w.x - v.x) + (p.y - v.y) * (w.y - v.y)) / l2;
    t = Math.max(0, Math.min(1, t));
    var pt1 = { x: p.x, y: p.y }
    var pt2 = { x: v.x + t * (w.x - v.x), y: v.y + t * (w.y - v.y) }
    var line = { pt1, pt2 }
    dist = Math.sqrt(dist2(p, { x: v.x + t * (w.x - v.x),
        y: v.y + t * (w.y - v.y) }))
    return {line, dist}
}
// function distToSegment(p, v, w, s) {
//     return Math.sqrt(distToSegmentSquared(p, v, w, s));
// }

// Main function invokation
$(document).ready(function () {
    main();
});
