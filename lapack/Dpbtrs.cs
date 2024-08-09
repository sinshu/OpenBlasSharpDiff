using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DPBTRS solves a system of linear equations A*X = B with a symmetric
        /// positive definite band matrix A using the Cholesky factorization
        /// A = U**T*U or A = L*L**T computed by DPBTRF.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangular factor stored in AB;
        /// = &#39;L&#39;:  Lower triangular factor stored in AB.
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
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in] AB is DOUBLE PRECISION array, dimension (LDAB,N).
        /// The triangular factor U or L from the Cholesky factorization
        /// A = U**T*U or A = L*L**T of the band matrix A, stored in the
        /// first KD+1 rows of the array.  The j-th column of U or L is
        /// stored in the j-th column of the array AB as follows:
        /// if UPLO =&#39;U&#39;, AB(kd+1+i-j,j) = U(i,j) for max(1,j-kd)&lt;=i&lt;=j;
        /// if UPLO =&#39;L&#39;, AB(1+i-j,j)    = L(i,j) for j&lt;=i&lt;=min(n,j+kd).
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KD+1.
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dpbtrs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dpbtrs(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int kd,
            int nrhs,
            double* ab,
            int ldab,
            double* b,
            int ldb);
    }
}
