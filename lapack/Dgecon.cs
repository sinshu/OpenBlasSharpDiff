using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGECON estimates the reciprocal of the condition number of a general
        /// real matrix A, in either the 1-norm or the infinity-norm, using
        /// the LU factorization computed by DGETRF.
        /// </para>
        /// <para>
        /// An estimate is obtained for norm(inv(A)), and the reciprocal of the
        /// condition number is computed as
        ///    RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="norm">
        /// [in] NORM is CHARACTER*1.
        /// Specifies whether the 1-norm condition number or the
        /// infinity-norm condition number is required:
        /// = &#39;1&#39; or &#39;O&#39;:  1-norm;
        /// = &#39;I&#39;:         Infinity-norm.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// The factors L and U from the factorization A = P*L*U
        /// as computed by DGETRF.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="anorm">
        /// [in] ANORM is DOUBLE PRECISION.
        /// If NORM = &#39;1&#39; or &#39;O&#39;, the 1-norm of the original matrix A.
        /// If NORM = &#39;I&#39;, the infinity-norm of the original matrix A.
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is DOUBLE PRECISION.
        /// The reciprocal of the condition number of the matrix A,
        /// computed as RCOND = 1/(norm(A) * norm(inv(A))).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// NaNs are illegal values for ANORM, and they propagate to
        /// the output parameter RCOND.
        /// Infinity is illegal for ANORM, and it propagates to the output
        /// parameter RCOND as 0.
        /// = 1:  if RCOND = NaN, or
        /// RCOND = Inf, or
        /// the computed norm of the inverse of A is 0.
        /// In the latter, RCOND = 0 is returned.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgecon", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgecon(
            MatrixLayout matrixLayout,
            char norm,
            int n,
            double* a,
            int lda,
            double anorm,
            double* rcond);
    }
}
