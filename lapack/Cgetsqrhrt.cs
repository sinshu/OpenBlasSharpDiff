using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGETSQRHRT computes a NB2-sized column blocked QR-factorization
        /// of a complex M-by-N matrix A with M &gt;= N,
        /// </para>
        /// <para>
        ///    A = Q * R.
        /// </para>
        /// <para>
        /// The routine uses internally a NB1-sized column blocked and MB1-sized
        /// row blocked TSQR-factorization and perfors the reconstruction
        /// of the Householder vectors from the TSQR output. The routine also
        /// converts the R_tsqr factor from the TSQR-factorization output into
        /// the R factor that corresponds to the Householder QR-factorization,
        /// </para>
        /// <para>
        ///    A = Q_tsqr * R_tsqr = Q * R.
        /// </para>
        /// <para>
        /// The output Q and R factors are stored in the same format as in CGEQRT
        /// (Q is in blocked compact WY-representation). See the documentation
        /// of CGEQRT for more details on the format.
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
        /// The number of columns of the matrix A. M &gt;= N &gt;= 0.
        /// </param>
        /// <param name="mb1">
        /// [in] MB1 is INTEGER.
        /// The row block size to be used in the blocked TSQR.
        /// MB1 &gt; N.
        /// </param>
        /// <param name="nb1">
        /// [in] NB1 is INTEGER.
        /// The column block size to be used in the blocked TSQR.
        /// N &gt;= NB1 &gt;= 1.
        /// </param>
        /// <param name="nb2">
        /// [in] NB2 is INTEGER.
        /// The block size to be used in the blocked QR that is
        /// output. NB2 &gt;= 1.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// 
        /// On entry: an M-by-N matrix A.
        /// 
        /// On exit:
        /// a) the elements on and above the diagonal
        /// of the array contain the N-by-N upper-triangular
        /// matrix R corresponding to the Householder QR;
        /// b) the elements below the diagonal represent Q by
        /// the columns of blocked V (compact WY-representation).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="t">
        /// [out] T is COMPLEX array, dimension (LDT,N)).
        /// The upper triangular block reflectors stored in compact form
        /// as a sequence of upper triangular blocks.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T.  LDT &gt;= NB2.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgetsqrhrt", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgetsqrhrt(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int mb1,
            int nb1,
            int nb2,
            Complex32* a,
            int lda,
            Complex32* t,
            int ldt);
    }
}
