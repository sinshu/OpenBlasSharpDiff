using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGEQR computes a QR factorization of a complex M-by-N matrix A:
        /// </para>
        /// <para>
        ///    A = Q * ( R ),
        ///            ( 0 )
        /// </para>
        /// <para>
        /// where:
        /// </para>
        /// <para>
        ///    Q is a M-by-M orthogonal matrix;
        ///    R is an upper-triangular N-by-N matrix;
        ///    0 is a (M-N)-by-N zero matrix, if M &gt; N.
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
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit, the elements on and above the diagonal of the array
        /// contain the min(M,N)-by-N upper trapezoidal matrix R
        /// (R is upper triangular if M &gt;= N);
        /// the elements below the diagonal are used to store part of the
        /// data structure to represent Q.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="t">
        /// [out] T is COMPLEX*16 array, dimension (MAX(5,TSIZE)).
        /// On exit, if INFO = 0, T(1) returns optimal (or either minimal
        /// or optimal, if query is assumed) TSIZE. See TSIZE for details.
        /// Remaining T contains part of the data structure used to represent Q.
        /// If one wants to apply or construct Q, then one needs to keep T
        /// (in addition to A) and pass it to further subroutines.
        /// </param>
        /// <param name="tsize">
        /// [in] TSIZE is INTEGER.
        /// If TSIZE &gt;= 5, the dimension of the array T.
        /// If TSIZE = -1 or -2, then a workspace query is assumed. The routine
        /// only calculates the sizes of the T and WORK arrays, returns these
        /// values as the first entries of the T and WORK arrays, and no error
        /// message related to T or WORK is issued by XERBLA.
        /// If TSIZE = -1, the routine calculates optimal size of T for the
        /// optimum performance and returns this value in T(1).
        /// If TSIZE = -2, the routine calculates minimal size of T and
        /// returns this value in T(1).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgeqr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgeqr(
            MatrixLayout matrixLayout,
            int m,
            int n,
            Complex* a,
            int lda,
            Complex* t,
            int tsize);
    }
}
