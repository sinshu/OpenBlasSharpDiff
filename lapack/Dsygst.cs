using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSYGST reduces a real symmetric-definite generalized eigenproblem
        /// to standard form.
        /// </para>
        /// <para>
        /// If ITYPE = 1, the problem is A*x = lambda*B*x,
        /// and A is overwritten by inv(U**T)*A*inv(U) or inv(L)*A*inv(L**T)
        /// </para>
        /// <para>
        /// If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
        /// B*A*x = lambda*x, and A is overwritten by U*A*U**T or L**T*A*L.
        /// </para>
        /// <para>
        /// B must have been previously factorized as U**T*U or L*L**T by DPOTRF.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="itype">
        /// [in] ITYPE is INTEGER.
        /// = 1: compute inv(U**T)*A*inv(U) or inv(L)*A*inv(L**T);
        /// = 2 or 3: compute U*A*U**T or L**T*A*L.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored and B is factored as
        /// U**T*U;
        /// = &#39;L&#39;:  Lower triangle of A is stored and B is factored as
        /// L*L**T.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices A and B.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the leading
        /// N-by-N upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// 
        /// On exit, if INFO = 0, the transformed matrix, stored in the
        /// same format as A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in] B is DOUBLE PRECISION array, dimension (LDB,N).
        /// The triangular factor from the Cholesky factorization of B,
        /// as returned by DPOTRF.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsygst", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsygst(
            MatrixLayout matrixLayout,
            int itype,
            char uplo,
            int n,
            double* a,
            int lda,
            double* b,
            int ldb);
    }
}
