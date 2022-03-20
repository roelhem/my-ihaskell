module Math.AD.Real.Properties where

import Math.AD.Data.Bracketing
import Math.AD.Real.FuncDescription
import Math.AD.Real.FuncDims

fmaMatProd :: FuncDimensions a => a -> a -> Integer
fmaMatProd lhs rhs = inDim rhs * inDim lhs * outDim lhs

fma :: FuncDescription a => Bracketing a -> Integer
fma Start = 0
fma (Tangent f Start) = inDim f * edgeCount f
fma (Adjoint Start f) = outDim f * edgeCount f
fma (Preacc Start rhs) = fma rhs
fma (Preacc lhs Start) = fma lhs
fma (Tangent f other) = fma other + inDim other * edgeCount f
fma (Adjoint other f) = fma other + outDim other * edgeCount f
fma (Preacc lhs rhs) = fma lhs + fma rhs + fmaMatProd lhs rhs

mem :: FuncDescription a => Bracketing a -> Integer
mem Start = 0
mem (Tangent f Start) = inDim f
mem (Tangent f other) = mem other
mem (Adjoint Start f) = outDim f + edgeCount f
mem (Adjoint other f) = mem other + edgeCount f
mem (Preacc lhs rhs) = max (mem lhs) (mem rhs) + outDim lhs * inDim rhs
