using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGEEQUB computes row and column scalings intended to equilibrate an
        /// M-by-N matrix A and reduce its condition number.  R returns the row
        /// scale factors and C the column scale factors, chosen to try to make
        /// the largest element in each row and column of the matrix B with
        /// elements B(i,j)=R(i)*A(i,j)*C(j) have an absolute value of at most
        /// the radix.
        /// </para>
        /// <para>
        /// R(i) and C(j) are restricted to be a power of the radix between
        /// SMLNUM = smallest safe number and BIGNUM = largest safe number.  Use
        /// of these scaling factors is not guaranteed to reduce the condition
        /// number of A but works well in practice.
        /// </para>
        /// <para>
        /// This routine differs from DGEEQU by restricting the scaling factors
        /// to a power of the radix.  Barring over- and underflow, scaling by
        /// these factors introduces no additional rounding errors.  However, the
        /// scaled entries&#39; magnitudes are no longer approximately 1 but lie
        /// between sqrt(radix) and 1/sqrt(radix).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// The M-by-N matrix whose equilibration factors are
        /// to be computed.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="r">
        /// [out] R is DOUBLE PRECISION array, dimension (M).
        /// If INFO = 0 or INFO &gt; M, R contains the row scale factors
        /// for A.
        /// </param>
        /// <param name="c">
        /// [out] C is DOUBLE PRECISION array, dimension (N).
        /// If INFO = 0,  C contains the column scale factors for A.
        /// </param>
        /// <param name="rowcnd">
        /// [out] ROWCND is DOUBLE PRECISION.
        /// If INFO = 0 or INFO &gt; M, ROWCND contains the ratio of the
        /// smallest R(i) to the largest R(i).  If ROWCND &gt;= 0.1 and
        /// AMAX is neither too large nor too small, it is not worth
        /// scaling by R.
        /// </param>
        /// <param name="colcnd">
        /// [out] COLCND is DOUBLE PRECISION.
        /// If INFO = 0, COLCND contains the ratio of the smallest
        /// C(i) to the largest C(i).  If COLCND &gt;= 0.1, it is not
        /// worth scaling by C.
        /// </param>
        /// <param name="amax">
        /// [out] AMAX is DOUBLE PRECISION.
        /// Absolute value of largest matrix element.  If AMAX is very
        /// close to overflow or very close to underflow, the matrix
        /// should be scaled.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i,  and i is
        /// &lt;= M:  the i-th row of A is exactly zero
        /// &gt;  M:  the (i-M)-th column of A is exactly zero
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgeequb", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgeequb(
            MatrixLayout matrixLayout,
            int m,
            int n,
            double* a,
            int lda,
            double* r,
            double* c,
            double* rowcnd,
            double* colcnd,
            double* amax);
    }
}
