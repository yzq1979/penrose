import { Tensor, Rank } from "@tensorflow/tfjs";
import { ICanvasSize } from "./types";
import { randFloat } from "./Util";

// Properties should be polymorphic over the type of numbers because we want to (1) optimize with Tensors and (2) render with floats

/**
 * Base properties of a `Circle` GPI.
 *
 * @template T type of floating point number. Defaults to `number`.
 */
interface ICircleProps<T = number> extends Properties<T> {
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

/**
 * Base properties of a `Text` GPI.
 * NOTE: When explicitly declared or computed in Style programs, w and h take precedence over fontSize. Therefore, custom fontSize in Style will only work when w and h are not specified or computed.
 * @template T type of floating point number. Defaults to `number`
 */
interface ITextProps<T = number> extends Properties<T> {
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
 * Base properties of a `Arrow` GPI.
 *
 * @template T type of floating point number. Defaults to `number`
 */
interface IArrowProps<T = number> extends Properties<T> {
  startX: IFloatV<T>;
  startY: IFloatV<T>;
  endX: IFloatV<T>;
  endY: IFloatV<T>;
  thickness: IFloatV<T>;
  style: IStrV;
  color: IColorV<T>;
  name: IStrV;
  rotation: IFloatV<T>;
  arrowheadStyle: IStrV;
  arrowheadSize: IFloatV<T>;
}

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

/**
 * Sample all properties of a shape
 *
 * @param {Shape} shape the old shape
 * @param {ICanvasSize} canvasSize the canvas size
 */
export const sampleShape = (shape: Shape, canvasSize: ICanvasSize) => ({
  ...shape,
  properties: sampleProperties(shape.shapeType, canvasSize),
});

/**
 * Given the shape type, sample all properties of this type of shape.
 * NOTE: since we are switching by shape type here, TS doesn't know
 *
 * @param {ShapeType} shapeType the type of the GPI (e.g. Circle)
 * @param {ICanvasSize} canvasSize the canvas size
 * @returns {Properties<number>} sampled properties
 */
const sampleProperties = (
  shapeType: ShapeType,
  canvasSize: ICanvasSize
): Properties<number> => {
  switch (shapeType) {
    case "Circle":
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
    case "Text":
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
    case "Arrow":
      return {
        startX: sampleX(canvasSize),
        startY: sampleY(canvasSize),
        endX: sampleX(canvasSize),
        endY: sampleY(canvasSize),
        thickness: sampleFloatIn(5, 15),
        style: strV("straight"),
        color: sampleColor(),
        name: strV("defaultArrow"),
        rotation: floatV(0.0),
        arrowheadStyle: strV("arrowhead-2"),
        arrowheadSize: floatV(1.0),
      };
    default:
      throw new Error(`${shapeType} cannot be resampled.`);
  }
};

const sampleFloatIn = (min: number, max: number) => floatV(randFloat(min, max));
const sampleX = (canvas: ICanvasSize): IFloatV<number> =>
  floatV(randFloat(-canvas.width / 2, canvas.width / 2));
const sampleY = (canvas: ICanvasSize): IFloatV<number> =>
  floatV(randFloat(-canvas.height / 2, canvas.height / 2));
const sampleWidth = (canvas: ICanvasSize): IFloatV<number> =>
  floatV(randFloat(3, canvas.width / 6));
const sampleHeight = (canvas: ICanvasSize): IFloatV<number> =>
  floatV(randFloat(3, canvas.height / 6));
const sampleStroke = (): IFloatV<number> => floatV(randFloat(0.5, 3));
const sampleColor = (): IColorV<number> => {
  const [min, max] = [0.1, 0.9];
  return makeRGBA(
    randFloat(min, max),
    randFloat(min, max),
    randFloat(min, max),
    0.5 // NOTE: a random color doesn't have a random opacity
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

////////////////////////////////////////////////////////////////////////////////
// Currently unused code below: class version of shapes
// NOTE: the issue with this version is that the standard JS dictionary data structure doesn't let you have overloaded functions with the same name, say `contains: (Circle, Circle)` vs. `contains: (Ciccle, Text)`.

/**
 * This interface inclides the base properties that are shared accross all shapes
 *
 * @interface IBaseProps
 * @template S type of properties of a specific shape
 */
// interface IBaseProps<S> {
//   // Common properties
//   shapeType: string; // unique identifier for this type of shape
//   // Common queries
//   sampleProperties(canvasSize: ICanvasSize): S; // get a newly sampled set of properties
// }

/**
 * The common queries to all shapes (e.g. finding the bounding box)
 *
 * @interface IBaseQueries
 * @template T the type of floating point numbers
 */
// interface IBaseQueries<T = Tensor> {
//   center(): [T, T]; // get the center of the shape
//   getBBox(): IBBox<T>; // get the bounding box of the shape
// }

// interface IBBox<T> {
//   x: T;
//   y: T;
//   width: T;
//   height: T;
// }

// class Circle implements IBaseProps<ICircleProps>, IBaseQueries<Tensor> {
//   public readonly shapeType: string = "Circle";
//   constructor(public props: ICircleProps) {}
//   public sampleProperties(canvasSize: ICanvasSize): ICircleProps<number> {
//     return {
//       x: sampleX(canvasSize),
//       y: sampleY(canvasSize),
//       r: sampleWidth(canvasSize),
//       strokeWidth: sampleStroke(),
//       strokeStyle: strV("solid"),
//       strokeColor: sampleColor(),
//       color: sampleColor(),
//       style: strV("filled"),
//       name: strV("defaultCircle"),
//     };
//   }
//   public center(): [Tensor, Tensor] {
//     throw new Error("Method not implemented.");
//   }
//   public getBBox(): IBBox<Tensor> {
//     throw new Error("Method not implemented.");
//   }
// }

// class Text implements IBaseProps<ITextProps>, IBaseQueries<Tensor> {
//   public readonly shapeType: string = "Text";
//   sampleProperties(canvasSize: ICanvasSize): ITextProps<number> {
//     return {
//       x: sampleX(canvasSize),
//       y: sampleY(canvasSize),
//       w: floatV(0), // NOTE: updated by front-end
//       h: floatV(0), // NOTE: updated by front-end
//       fontSize: strV("12pt"),
//       polygon: emptyPoly,
//       string: strV("defaultLabelText"),
//       rotation: floatV(0.0),
//       style: strV("none"),
//       stroke: strV("none"),
//       color: makeRGBA(0, 0, 0, 1),
//       name: strV("defaultCircle"),
//     };
//   }
//   center(): [Tensor<Rank>, Tensor<Rank>] {
//     throw new Error("Method not implemented.");
//   }
//   getBBox(): IBBox<Tensor<Rank>> {
//     throw new Error("Method not implemented.");
//   }
// }

// contains(c: Circle, c1: Circle);
// contains(c: Rectangle, c1: Circle);

// interface ILineLike<T> {
//   endPoints(): [T, T];
// }

// interface IPolygonizable<T> {
//   getPolygon(): IPolygonV<T>; // get a bounding polygon of the shape
// }
