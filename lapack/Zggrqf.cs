using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGGRQF computes a generalized RQ factorization of an M-by-N matrix A
        /// and a P-by-N matrix B:
        /// </para>
        /// <para>
        ///             A = R*Q,        B = Z*T*Q,
        /// </para>
        /// <para>
        /// where Q is an N-by-N unitary matrix, Z is a P-by-P unitary
        /// matrix, and R and T assume one of the forms:
        /// </para>
        /// <para>
        /// if M &lt;= N,  R = ( 0  R12 ) M,   or if M &gt; N,  R = ( R11 ) M-N,
        ///                  N-M  M                           ( R21 ) N
        ///                                                      N
        /// </para>
        /// <para>
        /// where R12 or R21 is upper triangular, and
        /// </para>
        /// <para>
        /// if P &gt;= N,  T = ( T11 ) N  ,   or if P &lt; N,  T = ( T11  T12 ) P,
        ///                 (  0  ) P-N                         P   N-P
        ///                    N
        /// </para>
        /// <para>
        /// where T11 is upper triangular.
        /// </para>
        /// <para>
        /// In particular, if B is square and nonsingular, the GRQ factorization
        /// of A and B implicitly gives the RQ factorization of A*inv(B):
        /// </para>
        /// <para>
        ///              A*inv(B) = (R*inv(T))*Z**H
        /// </para>
        /// <para>
        /// where inv(B) denotes the inverse of the matrix B, and Z**H denotes the
        /// conjugate transpose of the matrix Z.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="p">
        /// [in] P is INTEGER.
        /// The number of rows of the matrix B.  P &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrices A and B. N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit, if M &lt;= N, the upper triangle of the subarray
        /// A(1:M,N-M+1:N) contains the M-by-M upper triangular matrix R;
        /// if M &gt; N, the elements on and above the (M-N)-th subdiagonal
        /// contain the M-by-N upper trapezoidal matrix R; the remaining
        /// elements, with the array TAUA, represent the unitary
        /// matrix Q as a product of elementary reflectors (see Further
        /// Details).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,M).
        /// </param>
        /// <param name="taua">
        /// [out] TAUA is COMPLEX*16 array, dimension (min(M,N)).
        /// The scalar factors of the elementary reflectors which
        /// represent the unitary matrix Q (see Further Details).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,N).
        /// On entry, the P-by-N matrix B.
        /// On exit, the elements on and above the diagonal of the array
        /// contain the min(P,N)-by-N upper trapezoidal matrix T (T is
        /// upper triangular if P &gt;= N); the elements below the diagonal,
        /// with the array TAUB, represent the unitary matrix Z as a
        /// product of elementary reflectors (see Further Details).
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,P).
        /// </param>
        /// <param name="taub">
        /// [out] TAUB is COMPLEX*16 array, dimension (min(P,N)).
        /// The scalar factors of the elementary reflectors which
        /// represent the unitary matrix Z (see Further Details).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO=-i, the i-th argument had an illegal value.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The matrix Q is represented as a product of elementary reflectors
        /// </para>
        /// <para>
        ///     Q = H(1) H(2) . . . H(k), where k = min(m,n).
        /// </para>
        /// <para>
        ///  Each H(i) has the form
        /// </para>
        /// <para>
        ///     H(i) = I - taua * v * v**H
        /// </para>
        /// <para>
        ///  where taua is a complex scalar, and v is a complex vector with
        ///  v(n-k+i+1:n) = 0 and v(n-k+i) = 1; v(1:n-k+i-1) is stored on exit in
        ///  A(m-k+i,1:n-k+i-1), and taua in TAUA(i).
        ///  To form Q explicitly, use LAPACK subroutine ZUNGRQ.
        ///  To use Q to update another matrix, use LAPACK subroutine ZUNMRQ.
        /// </para>
        /// <para>
        ///  The matrix Z is represented as a product of elementary reflectors
        /// </para>
        /// <para>
        ///     Z = H(1) H(2) . . . H(k), where k = min(p,n).
        /// </para>
        /// <para>
        ///  Each H(i) has the form
        /// </para>
        /// <para>
        ///     H(i) = I - taub * v * v**H
        /// </para>
        /// <para>
        ///  where taub is a complex scalar, and v is a complex vector with
        ///  v(1:i-1) = 0 and v(i) = 1; v(i+1:p) is stored on exit in B(i+1:p,i),
        ///  and taub in TAUB(i).
        ///  To form Z explicitly, use LAPACK subroutine ZUNGQR.
        ///  To use Z to update another matrix, use LAPACK subroutine ZUNMQR.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zggrqf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zggrqf(
            MatrixLayout matrixLayout,
            int m,
            int p,
            int n,
            Complex* a,
            int lda,
            Complex* taua,
            Complex* b,
            int ldb,
            Complex* taub);
    }
}
