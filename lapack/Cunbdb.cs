using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CUNBDB simultaneously bidiagonalizes the blocks of an M-by-M
        /// partitioned unitary matrix X:
        /// </para>
        /// <para>
        ///                                 [ B11 | B12 0  0 ]
        ///     [ X11 | X12 ]   [ P1 |    ] [  0  |  0 -I  0 ] [ Q1 |    ]**H
        /// X = [-----------] = [---------] [----------------] [---------]   .
        ///     [ X21 | X22 ]   [    | P2 ] [ B21 | B22 0  0 ] [    | Q2 ]
        ///                                 [  0  |  0  0  I ]
        /// </para>
        /// <para>
        /// X11 is P-by-Q. Q must be no larger than P, M-P, or M-Q. (If this is
        /// not the case, then X must be transposed and/or permuted. This can be
        /// done in constant time using the TRANS and SIGNS options. See CUNCSD
        /// for details.)
        /// </para>
        /// <para>
        /// The unitary matrices P1, P2, Q1, and Q2 are P-by-P, (M-P)-by-
        /// (M-P), Q-by-Q, and (M-Q)-by-(M-Q), respectively. They are
        /// represented implicitly by Householder vectors.
        /// </para>
        /// <para>
        /// B11, B12, B21, and B22 are Q-by-Q bidiagonal matrices represented
        /// implicitly by angles THETA, PHI.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER.
        /// = &#39;T&#39;:      X, U1, U2, V1T, and V2T are stored in row-major
        /// order;
        /// otherwise:  X, U1, U2, V1T, and V2T are stored in column-
        /// major order.
        /// </param>
        /// <param name="signs">
        /// [in] SIGNS is CHARACTER.
        /// = &#39;O&#39;:      The lower-left block is made nonpositive (the
        /// &quot;other&quot; convention);
        /// otherwise:  The upper-right block is made nonpositive (the
        /// &quot;default&quot; convention).
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows and columns in X.
        /// </param>
        /// <param name="p">
        /// [in] P is INTEGER.
        /// The number of rows in X11 and X12. 0 &lt;= P &lt;= M.
        /// </param>
        /// <param name="q">
        /// [in] Q is INTEGER.
        /// The number of columns in X11 and X21. 0 &lt;= Q &lt;=
        /// MIN(P,M-P,M-Q).
        /// </param>
        /// <param name="x11">
        /// [in,out] X11 is COMPLEX array, dimension (LDX11,Q).
        /// On entry, the top-left block of the unitary matrix to be
        /// reduced. On exit, the form depends on TRANS:
        /// If TRANS = &#39;N&#39;, then
        /// the columns of tril(X11) specify reflectors for P1,
        /// the rows of triu(X11,1) specify reflectors for Q1;
        /// else TRANS = &#39;T&#39;, and
        /// the rows of triu(X11) specify reflectors for P1,
        /// the columns of tril(X11,-1) specify reflectors for Q1.
        /// </param>
        /// <param name="ldx11">
        /// [in] LDX11 is INTEGER.
        /// The leading dimension of X11. If TRANS = &#39;N&#39;, then LDX11 &gt;=
        /// P; else LDX11 &gt;= Q.
        /// </param>
        /// <param name="x12">
        /// [in,out] X12 is COMPLEX array, dimension (LDX12,M-Q).
        /// On entry, the top-right block of the unitary matrix to
        /// be reduced. On exit, the form depends on TRANS:
        /// If TRANS = &#39;N&#39;, then
        /// the rows of triu(X12) specify the first P reflectors for
        /// Q2;
        /// else TRANS = &#39;T&#39;, and
        /// the columns of tril(X12) specify the first P reflectors
        /// for Q2.
        /// </param>
        /// <param name="ldx12">
        /// [in] LDX12 is INTEGER.
        /// The leading dimension of X12. If TRANS = &#39;N&#39;, then LDX12 &gt;=
        /// P; else LDX11 &gt;= M-Q.
        /// </param>
        /// <param name="x21">
        /// [in,out] X21 is COMPLEX array, dimension (LDX21,Q).
        /// On entry, the bottom-left block of the unitary matrix to
        /// be reduced. On exit, the form depends on TRANS:
        /// If TRANS = &#39;N&#39;, then
        /// the columns of tril(X21) specify reflectors for P2;
        /// else TRANS = &#39;T&#39;, and
        /// the rows of triu(X21) specify reflectors for P2.
        /// </param>
        /// <param name="ldx21">
        /// [in] LDX21 is INTEGER.
        /// The leading dimension of X21. If TRANS = &#39;N&#39;, then LDX21 &gt;=
        /// M-P; else LDX21 &gt;= Q.
        /// </param>
        /// <param name="x22">
        /// [in,out] X22 is COMPLEX array, dimension (LDX22,M-Q).
        /// On entry, the bottom-right block of the unitary matrix to
        /// be reduced. On exit, the form depends on TRANS:
        /// If TRANS = &#39;N&#39;, then
        /// the rows of triu(X22(Q+1:M-P,P+1:M-Q)) specify the last
        /// M-P-Q reflectors for Q2,
        /// else TRANS = &#39;T&#39;, and
        /// the columns of tril(X22(P+1:M-Q,Q+1:M-P)) specify the last
        /// M-P-Q reflectors for P2.
        /// </param>
        /// <param name="ldx22">
        /// [in] LDX22 is INTEGER.
        /// The leading dimension of X22. If TRANS = &#39;N&#39;, then LDX22 &gt;=
        /// M-P; else LDX22 &gt;= M-Q.
        /// </param>
        /// <param name="theta">
        /// [out] THETA is REAL array, dimension (Q).
        /// The entries of the bidiagonal blocks B11, B12, B21, B22 can
        /// be computed from the angles THETA and PHI. See Further
        /// Details.
        /// </param>
        /// <param name="phi">
        /// [out] PHI is REAL array, dimension (Q-1).
        /// The entries of the bidiagonal blocks B11, B12, B21, B22 can
        /// be computed from the angles THETA and PHI. See Further
        /// Details.
        /// </param>
        /// <param name="taup1">
        /// [out] TAUP1 is COMPLEX array, dimension (P).
        /// The scalar factors of the elementary reflectors that define
        /// P1.
        /// </param>
        /// <param name="taup2">
        /// [out] TAUP2 is COMPLEX array, dimension (M-P).
        /// The scalar factors of the elementary reflectors that define
        /// P2.
        /// </param>
        /// <param name="tauq1">
        /// [out] TAUQ1 is COMPLEX array, dimension (Q).
        /// The scalar factors of the elementary reflectors that define
        /// Q1.
        /// </param>
        /// <param name="tauq2">
        /// [out] TAUQ2 is COMPLEX array, dimension (M-Q).
        /// The scalar factors of the elementary reflectors that define
        /// Q2.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The bidiagonal blocks B11, B12, B21, and B22 are represented
        ///  implicitly by angles THETA(1), ..., THETA(Q) and PHI(1), ...,
        ///  PHI(Q-1). B11 and B21 are upper bidiagonal, while B21 and B22 are
        ///  lower bidiagonal. Every entry in each bidiagonal band is a product
        ///  of a sine or cosine of a THETA with a sine or cosine of a PHI. See
        ///  [1] or CUNCSD for details.
        /// </para>
        /// <para>
        ///  P1, P2, Q1, and Q2 are represented as products of elementary
        ///  reflectors. See CUNCSD for details on generating P1, P2, Q1, and Q2
        ///  using CUNGQR and CUNGLQ.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cunbdb", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cunbdb(
            MatrixLayout matrixLayout,
            char trans,
            char signs,
            int m,
            int p,
            int q,
            Complex32* x11,
            int ldx11,
            Complex32* x12,
            int ldx12,
            Complex32* x21,
            int ldx21,
            Complex32* x22,
            int ldx22,
            float* theta,
            float* phi,
            Complex32* taup1,
            Complex32* taup2,
            Complex32* tauq1,
            Complex32* tauq2);
    }
}
