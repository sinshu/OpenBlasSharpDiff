using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGBCON estimates the reciprocal of the condition number of a real
        /// general band matrix A, in either the 1-norm or the infinity-norm,
        /// using the LU factorization computed by DGBTRF.
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
        /// Details of the LU factorization of the band matrix A, as
        /// computed by DGBTRF.  U is stored as an upper triangular band
        /// matrix with KL+KU superdiagonals in rows 1 to KL+KU+1, and
        /// the multipliers used during the factorization are stored in
        /// rows KL+KU+2 to 2*KL+KU+1.
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= 2*KL+KU+1.
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// The pivot indices; for 1 &lt;= i &lt;= N, row i of the matrix was
        /// interchanged with row IPIV(i).
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
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgbcon", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgbcon(
            MatrixLayout matrixLayout,
            char norm,
            int n,
            int kl,
            int ku,
            double* ab,
            int ldab,
            int* ipiv,
            double anorm,
            double* rcond);
    }
}
