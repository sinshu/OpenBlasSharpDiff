using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DBDSQR computes the singular values and, optionally, the right and/or
        /// left singular vectors from the singular value decomposition (SVD) of
        /// a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
        /// zero-shift QR algorithm.  The SVD of B has the form
        /// </para>
        /// <para>
        ///    B = Q * S * P**T
        /// </para>
        /// <para>
        /// where S is the diagonal matrix of singular values, Q is an orthogonal
        /// matrix of left singular vectors, and P is an orthogonal matrix of
        /// right singular vectors.  If left singular vectors are requested, this
        /// subroutine actually returns U*Q instead of Q, and, if right singular
        /// vectors are requested, this subroutine returns P**T*VT instead of
        /// P**T, for given real input matrices U and VT.  When U and VT are the
        /// orthogonal matrices that reduce a general matrix A to bidiagonal
        /// form:  A = U*B*VT, as computed by DGEBRD, then
        /// </para>
        /// <para>
        ///    A = (U*Q) * S * (P**T*VT)
        /// </para>
        /// <para>
        /// is the SVD of A.  Optionally, the subroutine may also compute Q**T*C
        /// for a given real input matrix C.
        /// </para>
        /// <para>
        /// See &quot;Computing  Small Singular Values of Bidiagonal Matrices With
        /// Guaranteed High Relative Accuracy,&quot; by J. Demmel and W. Kahan,
        /// LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
        /// no. 5, pp. 873-912, Sept 1990) and
        /// &quot;Accurate singular values and differential qd algorithms,&quot; by
        /// B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
        /// Department, University of California at Berkeley, July 1992
        /// for a detailed description of the algorithm.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  B is upper bidiagonal;
        /// = &#39;L&#39;:  B is lower bidiagonal.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix B.  N &gt;= 0.
        /// </param>
        /// <param name="ncvt">
        /// [in] NCVT is INTEGER.
        /// The number of columns of the matrix VT. NCVT &gt;= 0.
        /// </param>
        /// <param name="nru">
        /// [in] NRU is INTEGER.
        /// The number of rows of the matrix U. NRU &gt;= 0.
        /// </param>
        /// <param name="ncc">
        /// [in] NCC is INTEGER.
        /// The number of columns of the matrix C. NCC &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in,out] D is DOUBLE PRECISION array, dimension (N).
        /// On entry, the n diagonal elements of the bidiagonal matrix B.
        /// On exit, if INFO=0, the singular values of B in decreasing
        /// order.
        /// </param>
        /// <param name="e">
        /// [in,out] E is DOUBLE PRECISION array, dimension (N-1).
        /// On entry, the N-1 offdiagonal elements of the bidiagonal
        /// matrix B.
        /// On exit, if INFO = 0, E is destroyed; if INFO &gt; 0, D and E
        /// will contain the diagonal and superdiagonal elements of a
        /// bidiagonal matrix orthogonally equivalent to the one given
        /// as input.
        /// </param>
        /// <param name="vt">
        /// [in,out] VT is DOUBLE PRECISION array, dimension (LDVT, NCVT).
        /// On entry, an N-by-NCVT matrix VT.
        /// On exit, VT is overwritten by P**T * VT.
        /// Not referenced if NCVT = 0.
        /// </param>
        /// <param name="ldvt">
        /// [in] LDVT is INTEGER.
        /// The leading dimension of the array VT.
        /// LDVT &gt;= max(1,N) if NCVT &gt; 0; LDVT &gt;= 1 if NCVT = 0.
        /// </param>
        /// <param name="u">
        /// [in,out] U is DOUBLE PRECISION array, dimension (LDU, N).
        /// On entry, an NRU-by-N matrix U.
        /// On exit, U is overwritten by U * Q.
        /// Not referenced if NRU = 0.
        /// </param>
        /// <param name="ldu">
        /// [in] LDU is INTEGER.
        /// The leading dimension of the array U.  LDU &gt;= max(1,NRU).
        /// </param>
        /// <param name="c">
        /// [in,out] C is DOUBLE PRECISION array, dimension (LDC, NCC).
        /// On entry, an N-by-NCC matrix C.
        /// On exit, C is overwritten by Q**T * C.
        /// Not referenced if NCC = 0.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C.
        /// LDC &gt;= max(1,N) if NCC &gt; 0; LDC &gt;=1 if NCC = 0.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  If INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:
        /// if NCVT = NRU = NCC = 0,
        /// = 1, a split was marked by a positive value in E
        /// = 2, current block of Z not diagonalized after 30*N
        /// iterations (in inner while loop)
        /// = 3, termination criterion of outer while loop not met
        /// (program created more than N unreduced blocks)
        /// else NCVT = NRU = NCC = 0,
        /// the algorithm did not converge; D and E contain the
        /// elements of a bidiagonal matrix which is orthogonally
        /// similar to the input matrix B;  if INFO = i, i
        /// elements of E have not converged to zero.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dbdsqr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dbdsqr(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int ncvt,
            int nru,
            int ncc,
            double* d,
            double* e,
            double* vt,
            int ldvt,
            double* u,
            int ldu,
            double* c,
            int ldc);
    }
}
