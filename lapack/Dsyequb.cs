using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSYEQUB computes row and column scalings intended to equilibrate a
        /// symmetric matrix A (with respect to the Euclidean norm) and reduce
        /// its condition number. The scale factors S are computed by the BIN
        /// algorithm (see references) so that the scaled matrix B with elements
        /// B(i,j) = S(i)*A(i,j)*S(j) has a condition number within a factor N of
        /// the smallest possible condition number over all possible diagonal
        /// scalings.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A. N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// The N-by-N symmetric matrix whose scaling factors are to be
        /// computed.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,N).
        /// </param>
        /// <param name="s">
        /// [out] S is DOUBLE PRECISION array, dimension (N).
        /// If INFO = 0, S contains the scale factors for A.
        /// </param>
        /// <param name="scond">
        /// [out] SCOND is DOUBLE PRECISION.
        /// If INFO = 0, S contains the ratio of the smallest S(i) to
        /// the largest S(i). If SCOND &gt;= 0.1 and AMAX is neither too
        /// large nor too small, it is not worth scaling by S.
        /// </param>
        /// <param name="amax">
        /// [out] AMAX is DOUBLE PRECISION.
        /// Largest absolute value of any matrix element. If AMAX is
        /// very close to overflow or very close to underflow, the
        /// matrix should be scaled.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the i-th diagonal element is nonpositive.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsyequb", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsyequb(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            double* a,
            int lda,
            double* s,
            double* scond,
            double* amax);
    }
}
