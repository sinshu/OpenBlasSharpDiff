using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGETRI computes the inverse of a matrix using the LU factorization
        /// computed by DGETRF.
        /// </para>
        /// <para>
        /// This method inverts U and then computes inv(A) by solving the system
        /// inv(A)*L = inv(U) for inv(A).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the factors L and U from the factorization
        /// A = P*L*U as computed by DGETRF.
        /// On exit, if INFO = 0, the inverse of the original matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// The pivot indices from DGETRF; for 1&lt;=i&lt;=N, row i of the
        /// matrix was interchanged with row IPIV(i).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, U(i,i) is exactly zero; the matrix is
        /// singular and its inverse could not be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgetri", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgetri(
            MatrixLayout matrixLayout,
            int n,
            double* a,
            int lda,
            int* ipiv);
    }
}
