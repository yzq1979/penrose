import { Tensor, Rank } from "@tensorflow/tfjs";
import { ICanvasSize } from "./types";
import { canvasSize } from "./Canvas";
import { randFloat } from "./Util";
import Polygon from "./Polygon";

// Properties should be polymorphic over the type of numbers because we want to (1) optimize with Tensors and (2) render with floats

/**
 * Base properties of a `Circle` GPI.
 *
 * @interface ICircleProps
 * @template T type of floating point number. Defaults to `number`.
 */
interface ICircleProps<T = number> {
  x: IFloatV<T>;
  y: IFloatV<T>;
  r: IFloatV<T>;
  strokeWidth: IFloatV<T>;
  style: IStrV;
  strokeStyle: IStrV;
  strokeColor: IColorV<T>;
  color: IColorV<T>;
  name: IStrV;
}

interface ITextProps<T = number> {
  x: IFloatV<T>;
  y: IFloatV<T>;
  w: IFloatV<T>;
  h: IFloatV<T>;
  fontSize: IStrV;
  polygon: IPolygonV<T>;
  string: IStrV;
  rotation: IFloatV<T>;
  style: IStrV;
  stroke: IStrV;
  color: IColorV<T>;
  name: IStrV;
}

/**
 * This interface inclides the base properties that are shared accross all shapes
 *
 * @interface IBaseProps
 * @template S type of properties of a specific shape
 */
interface IBaseProps<S> {
  // Common properties
  shapeType: string; // unique identifier for this type of shape
  // Common queries
  sampleProperties(canvasSize: ICanvasSize): S; // get a newly sampled set of properties
}

/**
 * The common queries to all shapes (e.g. finding the bounding box)
 *
 * @interface IBaseQueries
 * @template T the type of floating point numbers
 */
interface IBaseQueries<T = Tensor> {
  center(): [T, T]; // get the center of the shape
  getBBox(): IBBox<T>; // get the bounding box of the shape
}

interface IBBox<T> {
  x: T;
  y: T;
  width: T;
  height: T;
}

class Circle implements IBaseProps<ICircleProps>, IBaseQueries<Tensor> {
  public readonly shapeType: string = "Circle";
  constructor(public props: ICircleProps) {}
  sampleProperties(canvasSize: ICanvasSize): ICircleProps<number> {
    return {
      x: sampleX(canvasSize),
      y: sampleY(canvasSize),
      r: sampleWidth(canvasSize),
      strokeWidth: sampleStroke(),
      strokeStyle: strV("solid"),
      strokeColor: sampleColor(),
      color: sampleColor(),
      style: strV("filled"),
      name: strV("defaultCircle"),
    };
  }
  center(): [Tensor, Tensor] {
    throw new Error("Method not implemented.");
  }
  getBBox(): IBBox<Tensor> {
    throw new Error("Method not implemented.");
  }
}

class Text implements IBaseProps<ITextProps>, IBaseQueries<Tensor> {
  shapeType: string;
  sampleProperties(canvasSize: ICanvasSize): ITextProps<number> {
    return {
      x: sampleX(canvasSize),
      y: sampleY(canvasSize),
      w: floatV(0), // NOTE: updated by front-end
      h: floatV(0), // NOTE: updated by front-end
      fontSize: strV("12pt"),
      polygon: emptyPoly,
      string: strV("defaultLabelText"),
      rotation: floatV(0.0),
      style: strV("none"),
      stroke: strV("none"),
      color: makeRGBA(0, 0, 0, 1),
      name: strV("defaultCircle"),
    };
  }
  center(): [Tensor<Rank>, Tensor<Rank>] {
    throw new Error("Method not implemented.");
  }
  getBBox(): IBBox<Tensor<Rank>> {
    throw new Error("Method not implemented.");
  }
}

interface ILineLike<T> {
  endPoints(): [T, T];
}

interface IPolygonizable<T> {
  getPolygon(): IPolygonV<T>; // get a bounding polygon of the shape
}

const sampleX = (canvas: ICanvasSize): IFloatV<number> =>
  floatV(randFloat(-canvas.width / 2, -canvas.width / 2));
const sampleY = (canvas: ICanvasSize): IFloatV<number> =>
  floatV(randFloat(-canvas.height / 2, -canvas.height / 2));
const sampleWidth = (canvas: ICanvasSize): IFloatV<number> =>
  floatV(randFloat(3, canvas.width / 6));
const sampleHeight = (canvas: ICanvasSize): IFloatV<number> =>
  floatV(randFloat(3, canvas.height / 6));
const sampleStroke = (): IFloatV<number> => floatV(randFloat(0.5, 3));
const sampleColor = (): IColorV<number> => {
  const [min, max] = [0.1, 0.9];
  // NOTE: a random number doesn't have a random opacity
  return makeRGBA(
    randFloat(min, max),
    randFloat(min, max),
    randFloat(min, max),
    0.5
  );
};

const makeRGBA = (
  r: number,
  g: number,
  b: number,
  a: number
): IColorV<number> => ({
  tag: "ColorV",
  contents: {
    tag: "RGBA",
    contents: [r, g, b, a],
  },
});

const emptyPoly: IPolygonV<number> = {
  tag: "PolygonV",
  contents: [
    [],
    [],
    [
      [NaN, NaN],
      [-NaN, -NaN],
    ],
    [],
  ],
};
const floatV = (num: number): IFloatV<number> => ({
  tag: "FloatV",
  contents: num,
});
const strV = (s: string): IStrV => ({ tag: "StrV", contents: s });

// ellipseType =
//   ( "Ellipse"
//   , M.fromList
//       [ ("x", (FloatT, x_sampler))
//       , ("y", (FloatT, y_sampler))
//       , ("rx", (FloatT, width_sampler))
//       , ("ry", (FloatT, height_sampler))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("strokeWidth", (FloatT, stroke_sampler))
//       , ("style", (StrT, sampleDiscrete [StrV "filled"]))
//       , ("strokeColor", (ColorT, sampleColor))
//       , ("strokeStyle", (StrT, stroke_style_sampler))
//       , ("color", (ColorT, sampleColor))
//       , ("name", (StrT, constValue $ StrV "defaultEllipse"))
//       ])

// -- When explicitly declared or computed in Style programs, w and h take precedence over fontSize.
// -- Therefore, custom fontSize in Style will only work when w and h are not specified or computed.
// textType =
//   ( "Text"
//   , M.fromList
//       [ ("x", (FloatT, sampleFloatIn (-canvasWidth / 2, canvasWidth / 2)))
//       , ("y", (FloatT, sampleFloatIn (-canvasHeight / 2, canvasHeight / 2)))
//       , ("w", (FloatT, constValue $ FloatV 0)) -- NOTE: updated by front-end
//       , ("h", (FloatT, constValue $ FloatV 0)) -- NOTE: updated by front-end
//       , ("fontSize", (StrT, constValue $ StrV "12pt"))
//       , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly)) -- Computed
//       , ("string", (StrT, constValue $ StrV "defaultLabelText"))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("style", (StrT, constValue $ StrV "none"))
//       , ("stroke", (StrT, constValue $ StrV "none"))
//       , ("color", (ColorT, constValue $ ColorV black))
//       , ("name", (StrT, constValue $ StrV "defaultCircle"))
//       ])

// arrowType =
//   ( "Arrow"
//   , M.fromList
//       [ ("startX", (FloatT, x_sampler))
//       , ("startY", (FloatT, y_sampler))
//       , ("endX", (FloatT, x_sampler))
//       , ("endY", (FloatT, y_sampler))
//       , ("thickness", (FloatT, sampleFloatIn (5, 15)))
//       , ("style", (StrT, constValue $ StrV "straight"))
//       , ("color", (ColorT, sampleColor))
//       , ("name", (StrT, constValue $ StrV "defaultArrow"))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("arrowheadStyle", (StrT, constValue $ StrV "arrowhead-2"))
//       , ("arrowheadSize", (FloatT, constValue $ FloatV 1.0))
//       ])

// braceType =
//   ( "Brace"
//   , M.fromList
//       [ ("startX", (FloatT, x_sampler))
//       , ("startY", (FloatT, y_sampler))
//       , ("endX", (FloatT, x_sampler))
//       , ("endY", (FloatT, y_sampler))
//       , ("color", (ColorT, sampleColor))
//       , ("thickness", (FloatT, sampleFloatIn (1, 6)))
//       , ("name", (StrT, constValue $ StrV "defaultBrace"))
//       ])

// curveType =
//   ( "Curve"
//   , M.fromList
//         -- These two fields are for storage.
//       [ ("path", (PtListT, constValue $ PtListV [])) -- TODO: sample path
//       , ("polyline", (PtListT, constValue $ PtListV [])) -- TODO: sample path
//       -- Computed
//       , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly))
//         -- The frontend only uses pathData to draw the curve.
//       , ("pathData", (PathDataT, constValue $ PathDataV [])) -- TODO: sample path
//       , ("strokeWidth", (FloatT, stroke_sampler))
//       , ("style", (StrT, constValue $ StrV "solid"))
//       , ("effect", (StrT, constValue $ StrV "none"))
//       , ("fill", (ColorT, sampleColor)) -- for no fill, set opacity to 0
//       , ("color", (ColorT, sampleColor))
//       , ("leftArrowhead", (BoolT, constValue $ BoolV False))
//       , ("rightArrowhead", (BoolT, constValue $ BoolV False))
//       , ("arrowheadStyle", (StrT, constValue $ StrV "arrowhead-2"))
//       , ("arrowheadSize", (FloatT, constValue $ FloatV 1.0))
//       , ("name", (StrT, constValue $ StrV "defaultCurve"))
//       ])

// lineType =
//   ( "Line"
//   , M.fromList
//       [ ("startX", (FloatT, x_sampler))
//       , ("startY", (FloatT, y_sampler))
//       , ("endX", (FloatT, x_sampler))
//       , ("endY", (FloatT, y_sampler))
//       , ("thickness", (FloatT, sampleFloatIn (5, 15)))
//       , ("leftArrowhead", (BoolT, constValue $ BoolV False))
//       , ("rightArrowhead", (BoolT, constValue $ BoolV False))
//       , ("arrowheadStyle", (BoolT, constValue $ StrV "arrowhead-2"))
//       , ("arrowheadSize", (BoolT, constValue $ FloatV 1.0))
//       , ("color", (ColorT, sampleColor))
//       , ("style", (StrT, constValue $ StrV "solid"))
//       , ("stroke", (StrT, constValue $ StrV "none"))
//       , ("name", (StrT, constValue $ StrV "defaultLine"))
//       ])

// rectType =
//   ( "Rectangle"
//   , M.fromList
//       [ ("x", (FloatT, x_sampler))
//       , ("y", (FloatT, y_sampler))
//       , ("w", (FloatT, width_sampler))
//       , ("h", (FloatT, height_sampler))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("color", (ColorT, sampleColor))
//       , ("strokeWidth", (FloatT, stroke_sampler))
//       , ("style", (StrT, constValue $ StrV "filled"))
//       , ("strokeColor", (ColorT, sampleColor))
//       , ("strokeStyle", (StrT, constValue $ StrV "none"))
//       , ("name", (StrT, constValue $ StrV "defaultRect"))
//       ])

// squareType =
//   ( "Square"
//   , M.fromList
//       [ ("x", (FloatT, x_sampler))
//       , ("y", (FloatT, y_sampler))
//       , ("side", (FloatT, width_sampler))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//         -- TODO: distinguish between stroke color and fill color everywhere
//       , ("color", (ColorT, sampleColor))
//       , ("style", (StrT, constValue $ StrV "none")) -- TODO: what is this?
//       , ("strokeColor", (ColorT, sampleColor))
//       , ("strokeWidth", (FloatT, constValue $ FloatV 0.0))
//       , ("name", (StrT, constValue $ StrV "defaultSquare"))
//       ])

// parallelogramType =
//   ( "Parallelogram"
//   , M.fromList
//       [ ("x", (FloatT, x_sampler)) -- (x, y) is the bottom-left corner of the parallelogram
//       , ("y", (FloatT, y_sampler))
//       , ("lengthX", (FloatT, width_sampler))
//       , ("lengthY", (FloatT, height_sampler))
//       , ("angle", (FloatT, constValue $ FloatV 0.0))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("color", (ColorT, sampleColor))
//       , ("stroke-style", (StrT, stroke_style_sampler))
//       , ("stroke-color", (ColorT, sampleColor))
//       , ("name", (StrT, constValue $ StrV "defaultParallelogram"))
//       ])

// imageType =
//   ( "Image"
//   , M.fromList
//       [ ("x", (FloatT, x_sampler))
//       , ("y", (FloatT, y_sampler))
//       , ("w", (FloatT, width_sampler))
//       , ("h", (FloatT, height_sampler))
//       , ("rotation", (FloatT, constValue $ FloatV 0.0))
//       , ("opacity", (FloatT, constValue $ FloatV 1.0))
//       , ("style", (StrT, constValue $ StrV "none"))
//       , ("stroke", (StrT, constValue $ StrV "none"))
//       , ("path", (StrT, constValue $ StrV "missing image path")) -- Absolute path (URL)
//       , ("name", (StrT, constValue $ StrV "defaultImage"))
//       ])
