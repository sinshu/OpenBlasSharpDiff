using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGGQRF computes a generalized QR factorization of an N-by-M matrix A
        /// and an N-by-P matrix B:
        /// </para>
        /// <para>
        ///             A = Q*R,        B = Q*T*Z,
        /// </para>
        /// <para>
        /// where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
        /// matrix, and R and T assume one of the forms:
        /// </para>
        /// <para>
        /// if N &gt;= M,  R = ( R11 ) M  ,   or if N &lt; M,  R = ( R11  R12 ) N,
        ///                 (  0  ) N-M                         N   M-N
        ///                    M
        /// </para>
        /// <para>
        /// where R11 is upper triangular, and
        /// </para>
        /// <para>
        /// if N &lt;= P,  T = ( 0  T12 ) N,   or if N &gt; P,  T = ( T11 ) N-P,
        ///                  P-N  N                           ( T21 ) P
        ///                                                      P
        /// </para>
        /// <para>
        /// where T12 or T21 is upper triangular.
        /// </para>
        /// <para>
        /// In particular, if B is square and nonsingular, the GQR factorization
        /// of A and B implicitly gives the QR factorization of inv(B)*A:
        /// </para>
        /// <para>
        ///              inv(B)*A = Z**T*(inv(T)*R)
        /// </para>
        /// <para>
        /// where inv(B) denotes the inverse of the matrix B, and Z**T denotes the
        /// transpose of the matrix Z.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of rows of the matrices A and B. N &gt;= 0.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of columns of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="p">
        /// [in] P is INTEGER.
        /// The number of columns of the matrix B.  P &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,M).
        /// On entry, the N-by-M matrix A.
        /// On exit, the elements on and above the diagonal of the array
        /// contain the min(N,M)-by-M upper trapezoidal matrix R (R is
        /// upper triangular if N &gt;= M); the elements below the diagonal,
        /// with the array TAUA, represent the orthogonal matrix Q as a
        /// product of min(N,M) elementary reflectors (see Further
        /// Details).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,N).
        /// </param>
        /// <param name="taua">
        /// [out] TAUA is DOUBLE PRECISION array, dimension (min(N,M)).
        /// The scalar factors of the elementary reflectors which
        /// represent the orthogonal matrix Q (see Further Details).
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,P).
        /// On entry, the N-by-P matrix B.
        /// On exit, if N &lt;= P, the upper triangle of the subarray
        /// B(1:N,P-N+1:P) contains the N-by-N upper triangular matrix T;
        /// if N &gt; P, the elements on and above the (N-P)-th subdiagonal
        /// contain the N-by-P upper trapezoidal matrix T; the remaining
        /// elements, with the array TAUB, represent the orthogonal
        /// matrix Z as a product of elementary reflectors (see Further
        /// Details).
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,N).
        /// </param>
        /// <param name="taub">
        /// [out] TAUB is DOUBLE PRECISION array, dimension (min(N,P)).
        /// The scalar factors of the elementary reflectors which
        /// represent the orthogonal matrix Z (see Further Details).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The matrix Q is represented as a product of elementary reflectors
        /// </para>
        /// <para>
        ///     Q = H(1) H(2) . . . H(k), where k = min(n,m).
        /// </para>
        /// <para>
        ///  Each H(i) has the form
        /// </para>
        /// <para>
        ///     H(i) = I - taua * v * v**T
        /// </para>
        /// <para>
        ///  where taua is a real scalar, and v is a real vector with
        ///  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i+1:n,i),
        ///  and taua in TAUA(i).
        ///  To form Q explicitly, use LAPACK subroutine DORGQR.
        ///  To use Q to update another matrix, use LAPACK subroutine DORMQR.
        /// </para>
        /// <para>
        ///  The matrix Z is represented as a product of elementary reflectors
        /// </para>
        /// <para>
        ///     Z = H(1) H(2) . . . H(k), where k = min(n,p).
        /// </para>
        /// <para>
        ///  Each H(i) has the form
        /// </para>
        /// <para>
        ///     H(i) = I - taub * v * v**T
        /// </para>
        /// <para>
        ///  where taub is a real scalar, and v is a real vector with
        ///  v(p-k+i+1:p) = 0 and v(p-k+i) = 1; v(1:p-k+i-1) is stored on exit in
        ///  B(n-k+i,1:p-k+i-1), and taub in TAUB(i).
        ///  To form Z explicitly, use LAPACK subroutine DORGRQ.
        ///  To use Z to update another matrix, use LAPACK subroutine DORMRQ.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dggqrf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dggqrf(
            MatrixLayout matrixLayout,
            int n,
            int m,
            int p,
            double* a,
            int lda,
            double* taua,
            double* b,
            int ldb,
            double* taub);
    }
}
