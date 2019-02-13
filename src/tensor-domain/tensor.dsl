tconstructor Index : type
tconstructor Dim : type
tconstructor Vector : type
tconstructor Matrix : type

-- you could unify vectors and matrices, and have vector as a type synonym for matrices with 1 column
-- could include scalars as types, and value constructors for building a vector or matrix out of scalars and vectors

operator AddVV [[n] : [Dim]] ([v, w] : [Vector(n), Vector(n)]) : Vector(n)
operator AddMM [[n, m] : [Dim, Dim]] ([A, B] : [Matrix(n, m), Matrix(n,m)]) : Matrix(n,m)
operator MulMV [[n, m] : [Dim, Dim]] ([A, x] : [Matrix(n, m), Vector(m)]) : Vector(n)
operator MulMM [[n, m, o] : [Dim, Dim]] ([A, B] : [Matrix(n, m), Matrix(m, o)]) : Matrix(n, o)

predicate SelectedElemV [[n] : [Dim]] ([v, i] : [Vector(n), Index]) : Prop
predicate SelectedElemM [[n, m] : [Dim, Dim]] ([A, i, j] : [Matrix(n, m), index, Index]) : Prop
predicate LessThan ([n, m] : [Dim, Dim]) : Prop
predicate Equal ([n, m] : [Dim, Dim]) : Prop

-- TODO: define notations and syntactic sugar
