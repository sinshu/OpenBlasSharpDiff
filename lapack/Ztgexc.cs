using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZTGEXC reorders the generalized Schur decomposition of a complex
        /// matrix pair (A,B), using an unitary equivalence transformation
        /// (A, B) := Q * (A, B) * Z**H, so that the diagonal block of (A, B) with
        /// row index IFST is moved to row ILST.
        /// </para>
        /// <para>
        /// (A, B) must be in generalized Schur canonical form, that is, A and
        /// B are both upper triangular.
        /// </para>
        /// <para>
        /// Optionally, the matrices Q and Z of generalized Schur vectors are
        /// updated.
        /// </para>
        /// <para>
        ///        Q(in) * A(in) * Z(in)**H = Q(out) * A(out) * Z(out)**H
        ///        Q(in) * B(in) * Z(in)**H = Q(out) * B(out) * Z(out)**H
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="wantq">
        /// [in] WANTQ is LOGICAL.
        /// .TRUE. : update the left transformation matrix Q;
        /// .FALSE.: do not update Q.
        /// </param>
        /// <param name="wantz">
        /// [in] WANTZ is LOGICAL.
        /// .TRUE. : update the right transformation matrix Z;
        /// .FALSE.: do not update Z.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices A and B. N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the upper triangular matrix A in the pair (A, B).
        /// On exit, the updated matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,N).
        /// On entry, the upper triangular matrix B in the pair (A, B).
        /// On exit, the updated matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,N).
        /// </param>
        /// <param name="q">
        /// [in,out] Q is COMPLEX*16 array, dimension (LDQ,N).
        /// On entry, if WANTQ = .TRUE., the unitary matrix Q.
        /// On exit, the updated matrix Q.
        /// If WANTQ = .FALSE., Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q. LDQ &gt;= 1;
        /// If WANTQ = .TRUE., LDQ &gt;= N.
        /// </param>
        /// <param name="z">
        /// [in,out] Z is COMPLEX*16 array, dimension (LDZ,N).
        /// On entry, if WANTZ = .TRUE., the unitary matrix Z.
        /// On exit, the updated matrix Z.
        /// If WANTZ = .FALSE., Z is not referenced.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z. LDZ &gt;= 1;
        /// If WANTZ = .TRUE., LDZ &gt;= N.
        /// </param>
        /// <param name="ifst">
        /// [in] IFST is INTEGER.
        /// </param>
        /// <param name="ilst">
        /// [in,out] ILST is INTEGER.
        /// Specify the reordering of the diagonal blocks of (A, B).
        /// The block with row index IFST is moved to row ILST, by a
        /// sequence of swapping between adjacent blocks.
        /// </param>
        /// <returns>
        /// =0:  Successful exit.
        /// &lt;0:  if INFO = -i, the i-th argument had an illegal value.
        /// =1:  The transformed matrix pair (A, B) would be too far
        /// from generalized Schur form; the problem is ill-
        /// conditioned. (A, B) may have been partially reordered,
        /// and ILST points to the first row of the current
        /// position of the block being moved.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ztgexc", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ztgexc(
            MatrixLayout matrixLayout,
            bool wantq,
            bool wantz,
            int n,
            Complex* a,
            int lda,
            Complex* b,
            int ldb,
            Complex* q,
            int ldq,
            Complex* z,
            int ldz,
            int ifst,
            int ilst);
    }
}
