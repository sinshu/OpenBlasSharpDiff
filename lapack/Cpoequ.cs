﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CPOEQU computes row and column scalings intended to equilibrate a
        /// Hermitian positive definite matrix A and reduce its condition number
        /// (with respect to the two-norm).  S contains the scale factors,
        /// S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
        /// elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
        /// choice of S puts the condition number of B within a factor N of the
        /// smallest possible condition number over all possible diagonal
        /// scalings.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX array, dimension (LDA,N).
        /// The N-by-N Hermitian positive definite matrix whose scaling
        /// factors are to be computed.  Only the diagonal elements of A
        /// are referenced.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="s">
        /// [out] S is REAL array, dimension (N).
        /// If INFO = 0, S contains the scale factors for A.
        /// </param>
        /// <param name="scond">
        /// [out] SCOND is REAL.
        /// If INFO = 0, S contains the ratio of the smallest S(i) to
        /// the largest S(i).  If SCOND &gt;= 0.1 and AMAX is neither too
        /// large nor too small, it is not worth scaling by S.
        /// </param>
        /// <param name="amax">
        /// [out] AMAX is REAL.
        /// Absolute value of largest matrix element.  If AMAX is very
        /// close to overflow or very close to underflow, the matrix
        /// should be scaled.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the i-th diagonal element is nonpositive.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cpoequ", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cpoequ(
            MatrixLayout matrixLayout,
            int n,
            Complex32* a,
            int lda,
            float* s,
            float* scond,
            float* amax);
    }
}
