using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZHETRS_AA_2STAGE solves a system of linear equations A*X = B with a 
        /// hermitian matrix A using the factorization A = U**H*T*U or
        /// A = L*T*L**H computed by ZHETRF_AA_2STAGE.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the details of the factorization are stored
        /// as an upper or lower triangular matrix.
        /// = &#39;U&#39;:  Upper triangular, form is A = U**H*T*U;
        /// = &#39;L&#39;:  Lower triangular, form is A = L*T*L**H.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX*16 array, dimension (LDA,N).
        /// Details of factors computed by ZHETRF_AA_2STAGE.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="tb">
        /// [out] TB is COMPLEX*16 array, dimension (LTB).
        /// Details of factors computed by ZHETRF_AA_2STAGE.
        /// </param>
        /// <param name="ltb">
        /// [in] LTB is INTEGER.
        /// The size of the array TB. LTB &gt;= 4*N.
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges as computed by
        /// ZHETRF_AA_2STAGE.
        /// </param>
        /// <param name="ipiv2">
        /// [in] IPIV2 is INTEGER array, dimension (N).
        /// Details of the interchanges as computed by
        /// ZHETRF_AA_2STAGE.
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,NRHS).
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zhetrs_aa_2stage", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo ZhetrsAa2stage(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            Complex* a,
            int lda,
            Complex* tb,
            int ltb,
            int* ipiv,
            int* ipiv2,
            Complex* b,
            int ldb);
    }
}
