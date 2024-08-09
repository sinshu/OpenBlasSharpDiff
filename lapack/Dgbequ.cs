using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGBEQU computes row and column scalings intended to equilibrate an
        /// M-by-N band matrix A and reduce its condition number.  R returns the
        /// row scale factors and C the column scale factors, chosen to try to
        /// make the largest element in each row and column of the matrix B with
        /// elements B(i,j)=R(i)*A(i,j)*C(j) have absolute value 1.
        /// </para>
        /// <para>
        /// R(i) and C(j) are restricted to be between SMLNUM = smallest safe
        /// number and BIGNUM = largest safe number.  Use of these scaling
        /// factors is not guaranteed to reduce the condition number of A but
        /// works well in practice.
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
        /// <param name="kl">
        /// [in] KL is INTEGER.
        /// The number of subdiagonals within the band of A.  KL &gt;= 0.
        /// </param>
        /// <param name="ku">
        /// [in] KU is INTEGER.
        /// The number of superdiagonals within the band of A.  KU &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in] AB is DOUBLE PRECISION array, dimension (LDAB,N).
        /// The band matrix A, stored in rows 1 to KL+KU+1.  The j-th
        /// column of A is stored in the j-th column of the array AB as
        /// follows:
        /// AB(ku+1+i-j,j) = A(i,j) for max(1,j-ku)&lt;=i&lt;=min(m,j+kl).
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KL+KU+1.
        /// </param>
        /// <param name="r">
        /// [out] R is DOUBLE PRECISION array, dimension (M).
        /// If INFO = 0, or INFO &gt; M, R contains the row scale factors
        /// for A.
        /// </param>
        /// <param name="c">
        /// [out] C is DOUBLE PRECISION array, dimension (N).
        /// If INFO = 0, C contains the column scale factors for A.
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
        /// &gt; 0:  if INFO = i, and i is
        /// &lt;= M:  the i-th row of A is exactly zero
        /// &gt;  M:  the (i-M)-th column of A is exactly zero
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgbequ", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgbequ(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int kl,
            int ku,
            double* ab,
            int ldab,
            double* r,
            double* c,
            double* rowcnd,
            double* colcnd,
            double* amax);
    }
}
