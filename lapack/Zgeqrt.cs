using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGEQRT computes a blocked QR factorization of a complex M-by-N matrix A
        /// using the compact WY representation of Q.
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
        /// <param name="nb">
        /// [in] NB is INTEGER.
        /// The block size to be used in the blocked QR.  MIN(M,N) &gt;= NB &gt;= 1.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit, the elements on and above the diagonal of the array
        /// contain the min(M,N)-by-N upper trapezoidal matrix R (R is
        /// upper triangular if M &gt;= N); the elements below the diagonal
        /// are the columns of V.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="t">
        /// [out] T is COMPLEX*16 array, dimension (LDT,MIN(M,N)).
        /// The upper triangular block reflectors stored in compact form
        /// as a sequence of upper triangular blocks.  See below
        /// for further details.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T.  LDT &gt;= NB.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The matrix V stores the elementary reflectors H(i) in the i-th column
        ///  below the diagonal. For example, if M=5 and N=3, the matrix V is
        /// </para>
        /// <para>
        ///               V = (  1       )
        ///                   ( v1  1    )
        ///                   ( v1 v2  1 )
        ///                   ( v1 v2 v3 )
        ///                   ( v1 v2 v3 )
        /// </para>
        /// <para>
        ///  where the vi&#39;s represent the vectors which define H(i), which are returned
        ///  in the matrix A.  The 1&#39;s along the diagonal of V are not stored in A.
        /// </para>
        /// <para>
        ///  Let K=MIN(M,N).  The number of blocks is B = ceiling(K/NB), where each
        ///  block is of order NB except for the last block, which is of order
        ///  IB = K - (B-1)*NB.  For each of the B blocks, a upper triangular block
        ///  reflector factor is computed: T1, T2, ..., TB.  The NB-by-NB (and IB-by-IB
        ///  for the last block) T&#39;s are stored in the NB-by-K matrix T as
        /// </para>
        /// <para>
        ///               T = (T1 T2 ... TB).
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgeqrt", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgeqrt(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int nb,
            Complex* a,
            int lda,
            Complex* t,
            int ldt);
    }
}
