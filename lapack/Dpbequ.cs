using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DPBEQU computes row and column scalings intended to equilibrate a
        /// symmetric positive definite band matrix A and reduce its condition
        /// number (with respect to the two-norm).  S contains the scale factors,
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
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangular of A is stored;
        /// = &#39;L&#39;:  Lower triangular of A is stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="kd">
        /// [in] KD is INTEGER.
        /// The number of superdiagonals of the matrix A if UPLO = &#39;U&#39;,
        /// or the number of subdiagonals if UPLO = &#39;L&#39;.  KD &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in] AB is DOUBLE PRECISION array, dimension (LDAB,N).
        /// The upper or lower triangle of the symmetric band matrix A,
        /// stored in the first KD+1 rows of the array.  The j-th column
        /// of A is stored in the j-th column of the array AB as follows:
        /// if UPLO = &#39;U&#39;, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AB(1+i-j,j)    = A(i,j) for j&lt;=i&lt;=min(n,j+kd).
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array A.  LDAB &gt;= KD+1.
        /// </param>
        /// <param name="s">
        /// [out] S is DOUBLE PRECISION array, dimension (N).
        /// If INFO = 0, S contains the scale factors for A.
        /// </param>
        /// <param name="scond">
        /// [out] SCOND is DOUBLE PRECISION.
        /// If INFO = 0, S contains the ratio of the smallest S(i) to
        /// the largest S(i).  If SCOND &gt;= 0.1 and AMAX is neither too
        /// large nor too small, it is not worth scaling by S.
        /// </param>
        /// <param name="amax">
        /// [out] AMAX is DOUBLE PRECISION.
        /// Absolute value of largest matrix element.  If AMAX is very
        /// close to overflow or very close to underflow, the matrix
        /// should be scaled.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  if INFO = i, the i-th diagonal element is nonpositive.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dpbequ", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dpbequ(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int kd,
            double* ab,
            int ldab,
            double* s,
            double* scond,
            double* amax);
    }
}
