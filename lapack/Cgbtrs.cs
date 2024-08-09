using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGBTRS solves a system of linear equations
        ///    A * X = B,  A**T * X = B,  or  A**H * X = B
        /// with a general band matrix A using the LU factorization computed
        /// by CGBTRF.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// Specifies the form of the system of equations.
        /// = &#39;N&#39;:  A * X = B     (No transpose)
        /// = &#39;T&#39;:  A**T * X = B  (Transpose)
        /// = &#39;C&#39;:  A**H * X = B  (Conjugate transpose)
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
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in] AB is COMPLEX array, dimension (LDAB,N).
        /// Details of the LU factorization of the band matrix A, as
        /// computed by CGBTRF.  U is stored as an upper triangular band
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
        /// <param name="b">
        /// [in,out] B is COMPLEX array, dimension (LDB,NRHS).
        /// On entry, the right hand side matrix B.
        /// On exit, the solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgbtrs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgbtrs(
            MatrixLayout matrixLayout,
            char trans,
            int n,
            int kl,
            int ku,
            int nrhs,
            Complex32* ab,
            int ldab,
            int* ipiv,
            Complex32* b,
            int ldb);
    }
}
