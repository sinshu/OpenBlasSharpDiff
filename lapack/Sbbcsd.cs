using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SBBCSD computes the CS decomposition of an orthogonal matrix in
        /// bidiagonal-block form,
        /// </para>
        /// <para>
        ///     [ B11 | B12 0  0 ]
        ///     [  0  |  0 -I  0 ]
        /// X = [----------------]
        ///     [ B21 | B22 0  0 ]
        ///     [  0  |  0  0  I ]
        /// </para>
        /// <para>
        ///                               [  C | -S  0  0 ]
        ///                   [ U1 |    ] [  0 |  0 -I  0 ] [ V1 |    ]**T
        ///                 = [---------] [---------------] [---------]   .
        ///                   [    | U2 ] [  S |  C  0  0 ] [    | V2 ]
        ///                               [  0 |  0  0  I ]
        /// </para>
        /// <para>
        /// X is M-by-M, its top-left block is P-by-Q, and Q must be no larger
        /// than P, M-P, or M-Q. (If Q is not the smallest index, then X must be
        /// transposed and/or permuted. This can be done in constant time using
        /// the TRANS and SIGNS options. See SORCSD for details.)
        /// </para>
        /// <para>
        /// The bidiagonal matrices B11, B12, B21, and B22 are represented
        /// implicitly by angles THETA(1:Q) and PHI(1:Q-1).
        /// </para>
        /// <para>
        /// The orthogonal matrices U1, U2, V1T, and V2T are input/output.
        /// The input matrices are pre- or post-multiplied by the appropriate
        /// singular vector matrices.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobu1">
        /// [in] JOBU1 is CHARACTER.
        /// = &#39;Y&#39;:      U1 is updated;
        /// otherwise:  U1 is not updated.
        /// </param>
        /// <param name="jobu2">
        /// [in] JOBU2 is CHARACTER.
        /// = &#39;Y&#39;:      U2 is updated;
        /// otherwise:  U2 is not updated.
        /// </param>
        /// <param name="jobv1t">
        /// [in] JOBV1T is CHARACTER.
        /// = &#39;Y&#39;:      V1T is updated;
        /// otherwise:  V1T is not updated.
        /// </param>
        /// <param name="jobv2t">
        /// [in] JOBV2T is CHARACTER.
        /// = &#39;Y&#39;:      V2T is updated;
        /// otherwise:  V2T is not updated.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER.
        /// = &#39;T&#39;:      X, U1, U2, V1T, and V2T are stored in row-major
        /// order;
        /// otherwise:  X, U1, U2, V1T, and V2T are stored in column-
        /// major order.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows and columns in X, the orthogonal matrix in
        /// bidiagonal-block form.
        /// </param>
        /// <param name="p">
        /// [in] P is INTEGER.
        /// The number of rows in the top-left block of X. 0 &lt;= P &lt;= M.
        /// </param>
        /// <param name="q">
        /// [in] Q is INTEGER.
        /// The number of columns in the top-left block of X.
        /// 0 &lt;= Q &lt;= MIN(P,M-P,M-Q).
        /// </param>
        /// <param name="theta">
        /// [in,out] THETA is REAL array, dimension (Q).
        /// On entry, the angles THETA(1),...,THETA(Q) that, along with
        /// PHI(1), ...,PHI(Q-1), define the matrix in bidiagonal-block
        /// form. On exit, the angles whose cosines and sines define the
        /// diagonal blocks in the CS decomposition.
        /// </param>
        /// <param name="phi">
        /// [in,out] PHI is REAL array, dimension (Q-1).
        /// The angles PHI(1),...,PHI(Q-1) that, along with THETA(1),...,
        /// THETA(Q), define the matrix in bidiagonal-block form.
        /// </param>
        /// <param name="u1">
        /// [in,out] U1 is REAL array, dimension (LDU1,P).
        /// On entry, a P-by-P matrix. On exit, U1 is postmultiplied
        /// by the left singular vector matrix common to [ B11 ; 0 ] and
        /// [ B12 0 0 ; 0 -I 0 0 ].
        /// </param>
        /// <param name="ldu1">
        /// [in] LDU1 is INTEGER.
        /// The leading dimension of the array U1, LDU1 &gt;= MAX(1,P).
        /// </param>
        /// <param name="u2">
        /// [in,out] U2 is REAL array, dimension (LDU2,M-P).
        /// On entry, an (M-P)-by-(M-P) matrix. On exit, U2 is
        /// postmultiplied by the left singular vector matrix common to
        /// [ B21 ; 0 ] and [ B22 0 0 ; 0 0 I ].
        /// </param>
        /// <param name="ldu2">
        /// [in] LDU2 is INTEGER.
        /// The leading dimension of the array U2, LDU2 &gt;= MAX(1,M-P).
        /// </param>
        /// <param name="v1t">
        /// [in,out] V1T is REAL array, dimension (LDV1T,Q).
        /// On entry, a Q-by-Q matrix. On exit, V1T is premultiplied
        /// by the transpose of the right singular vector
        /// matrix common to [ B11 ; 0 ] and [ B21 ; 0 ].
        /// </param>
        /// <param name="ldv1t">
        /// [in] LDV1T is INTEGER.
        /// The leading dimension of the array V1T, LDV1T &gt;= MAX(1,Q).
        /// </param>
        /// <param name="v2t">
        /// [in,out] V2T is REAL array, dimension (LDV2T,M-Q).
        /// On entry, an (M-Q)-by-(M-Q) matrix. On exit, V2T is
        /// premultiplied by the transpose of the right
        /// singular vector matrix common to [ B12 0 0 ; 0 -I 0 ] and
        /// [ B22 0 0 ; 0 0 I ].
        /// </param>
        /// <param name="ldv2t">
        /// [in] LDV2T is INTEGER.
        /// The leading dimension of the array V2T, LDV2T &gt;= MAX(1,M-Q).
        /// </param>
        /// <param name="b11d">
        /// [out] B11D is REAL array, dimension (Q).
        /// When SBBCSD converges, B11D contains the cosines of THETA(1),
        /// ..., THETA(Q). If SBBCSD fails to converge, then B11D
        /// contains the diagonal of the partially reduced top-left
        /// block.
        /// </param>
        /// <param name="b11e">
        /// [out] B11E is REAL array, dimension (Q-1).
        /// When SBBCSD converges, B11E contains zeros. If SBBCSD fails
        /// to converge, then B11E contains the superdiagonal of the
        /// partially reduced top-left block.
        /// </param>
        /// <param name="b12d">
        /// [out] B12D is REAL array, dimension (Q).
        /// When SBBCSD converges, B12D contains the negative sines of
        /// THETA(1), ..., THETA(Q). If SBBCSD fails to converge, then
        /// B12D contains the diagonal of the partially reduced top-right
        /// block.
        /// </param>
        /// <param name="b12e">
        /// [out] B12E is REAL array, dimension (Q-1).
        /// When SBBCSD converges, B12E contains zeros. If SBBCSD fails
        /// to converge, then B12E contains the subdiagonal of the
        /// partially reduced top-right block.
        /// </param>
        /// <param name="b21d">
        /// [out] B21D is REAL array, dimension (Q).
        /// When SBBCSD converges, B21D contains the negative sines of
        /// THETA(1), ..., THETA(Q). If SBBCSD fails to converge, then
        /// B21D contains the diagonal of the partially reduced bottom-left
        /// block.
        /// </param>
        /// <param name="b21e">
        /// [out] B21E is REAL array, dimension (Q-1).
        /// When SBBCSD converges, B21E contains zeros. If SBBCSD fails
        /// to converge, then B21E contains the subdiagonal of the
        /// partially reduced bottom-left block.
        /// </param>
        /// <param name="b22d">
        /// [out] B22D is REAL array, dimension (Q).
        /// When SBBCSD converges, B22D contains the negative sines of
        /// THETA(1), ..., THETA(Q). If SBBCSD fails to converge, then
        /// B22D contains the diagonal of the partially reduced bottom-right
        /// block.
        /// </param>
        /// <param name="b22e">
        /// [out] B22E is REAL array, dimension (Q-1).
        /// When SBBCSD converges, B22E contains zeros. If SBBCSD fails
        /// to converge, then B22E contains the subdiagonal of the
        /// partially reduced bottom-right block.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  if SBBCSD did not converge, INFO specifies the number
        /// of nonzero entries in PHI, and B11D, B11E, etc.,
        /// contain the partially reduced matrix.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sbbcsd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sbbcsd(
            MatrixLayout matrixLayout,
            char jobu1,
            char jobu2,
            char jobv1t,
            char jobv2t,
            char trans,
            int m,
            int p,
            int q,
            float* theta,
            float* phi,
            float* u1,
            int ldu1,
            float* u2,
            int ldu2,
            float* v1t,
            int ldv1t,
            float* v2t,
            int ldv2t,
            float* b11d,
            float* b11e,
            float* b12d,
            float* b12e,
            float* b21d,
            float* b21e,
            float* b22d,
            float* b22e);
    }
}
