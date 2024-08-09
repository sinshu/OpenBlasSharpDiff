using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZTZRZF reduces the M-by-N ( M&lt;=N ) complex upper trapezoidal matrix A
        /// to upper triangular form by means of unitary transformations.
        /// </para>
        /// <para>
        /// The upper trapezoidal matrix A is factored as
        /// </para>
        /// <para>
        ///    A = ( R  0 ) * Z,
        /// </para>
        /// <para>
        /// where Z is an N-by-N unitary matrix and R is an M-by-M upper
        /// triangular matrix.
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
        /// The number of columns of the matrix A.  N &gt;= M.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the leading M-by-N upper trapezoidal part of the
        /// array A must contain the matrix to be factorized.
        /// On exit, the leading M-by-M upper triangular part of A
        /// contains the upper triangular matrix R, and elements M+1 to
        /// N of the first M rows of A, with the array TAU, represent the
        /// unitary matrix Z as a product of M elementary reflectors.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="tau">
        /// [out] TAU is COMPLEX*16 array, dimension (M).
        /// The scalar factors of the elementary reflectors.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The N-by-N matrix Z can be computed by
        /// </para>
        /// <para>
        ///     Z =  Z(1)*Z(2)* ... *Z(M)
        /// </para>
        /// <para>
        ///  where each N-by-N Z(k) is given by
        /// </para>
        /// <para>
        ///     Z(k) = I - tau(k)*v(k)*v(k)**H
        /// </para>
        /// <para>
        ///  with v(k) is the kth row vector of the M-by-N matrix
        /// </para>
        /// <para>
        ///     V = ( I   A(:,M+1:N) )
        /// </para>
        /// <para>
        ///  I is the M-by-M identity matrix, A(:,M+1:N)
        ///  is the output stored in A on exit from ZTZRZF,
        ///  and tau(k) is the kth element of the array TAU.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ztzrzf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ztzrzf(
            MatrixLayout matrixLayout,
            int m,
            int n,
            Complex* a,
            int lda,
            Complex* tau);
    }
}
