using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSPTRS solves a system of linear equations A*X = B with a real
        /// symmetric matrix A stored in packed format using the factorization
        /// A = U*D*U**T or A = L*D*L**T computed by DSPTRF.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the details of the factorization are stored
        /// as an upper or lower triangular matrix.
        /// = &#39;U&#39;:  Upper triangular, form is A = U*D*U**T;
        /// = &#39;L&#39;:  Lower triangular, form is A = L*D*L**T.
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
        /// <param name="ap">
        /// [in] AP is DOUBLE PRECISION array, dimension (N*(N+1)/2).
        /// The block diagonal matrix D and the multipliers used to
        /// obtain the factor U or L as computed by DSPTRF, stored as a
        /// packed triangular matrix.
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the block structure of D
        /// as determined by DSPTRF.
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
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsptrs", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsptrs(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            double* ap,
            int* ipiv,
            double* b,
            int ldb);
    }
}
