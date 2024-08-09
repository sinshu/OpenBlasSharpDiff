using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// STGEXC reorders the generalized real Schur decomposition of a real
        /// matrix pair (A,B) using an orthogonal equivalence transformation
        /// </para>
        /// <para>
        ///                (A, B) = Q * (A, B) * Z**T,
        /// </para>
        /// <para>
        /// so that the diagonal block of (A, B) with row index IFST is moved
        /// to row ILST.
        /// </para>
        /// <para>
        /// (A, B) must be in generalized real Schur canonical form (as returned
        /// by SGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
        /// diagonal blocks. B is upper triangular.
        /// </para>
        /// <para>
        /// Optionally, the matrices Q and Z of generalized Schur vectors are
        /// updated.
        /// </para>
        /// <para>
        ///        Q(in) * A(in) * Z(in)**T = Q(out) * A(out) * Z(out)**T
        ///        Q(in) * B(in) * Z(in)**T = Q(out) * B(out) * Z(out)**T
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
        /// [in,out] A is REAL array, dimension (LDA,N).
        /// On entry, the matrix A in generalized real Schur canonical
        /// form.
        /// On exit, the updated matrix A, again in generalized
        /// real Schur canonical form.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is REAL array, dimension (LDB,N).
        /// On entry, the matrix B in generalized real Schur canonical
        /// form (A,B).
        /// On exit, the updated matrix B, again in generalized
        /// real Schur canonical form (A,B).
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,N).
        /// </param>
        /// <param name="q">
        /// [in,out] Q is REAL array, dimension (LDQ,N).
        /// On entry, if WANTQ = .TRUE., the orthogonal matrix Q.
        /// On exit, the updated matrix Q.
        /// If WANTQ = .FALSE., Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q. LDQ &gt;= 1.
        /// If WANTQ = .TRUE., LDQ &gt;= N.
        /// </param>
        /// <param name="z">
        /// [in,out] Z is REAL array, dimension (LDZ,N).
        /// On entry, if WANTZ = .TRUE., the orthogonal matrix Z.
        /// On exit, the updated matrix Z.
        /// If WANTZ = .FALSE., Z is not referenced.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z. LDZ &gt;= 1.
        /// If WANTZ = .TRUE., LDZ &gt;= N.
        /// </param>
        /// <param name="ifst">
        /// [in,out] IFST is INTEGER.
        /// </param>
        /// <param name="ilst">
        /// [in,out] ILST is INTEGER.
        /// Specify the reordering of the diagonal blocks of (A, B).
        /// The block with row index IFST is moved to row ILST, by a
        /// sequence of swapping between adjacent blocks.
        /// On exit, if IFST pointed on entry to the second row of
        /// a 2-by-2 block, it is changed to point to the first row;
        /// ILST always points to the first row of the block in its
        /// final position (which may differ from its input value by
        /// +1 or -1). 1 &lt;= IFST, ILST &lt;= N.
        /// </param>
        /// <returns>
        /// =0:  successful exit.
        /// &lt;0:  if INFO = -i, the i-th argument had an illegal value.
        /// =1:  The transformed matrix pair (A, B) would be too far
        /// from generalized Schur form; the problem is ill-
        /// conditioned. (A, B) may have been partially reordered,
        /// and ILST points to the first row of the current
        /// position of the block being moved.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_stgexc", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Stgexc(
            MatrixLayout matrixLayout,
            bool wantq,
            bool wantz,
            int n,
            float* a,
            int lda,
            float* b,
            int ldb,
            float* q,
            int ldq,
            float* z,
            int ldz,
            int* ifst,
            int* ilst);
    }
}
