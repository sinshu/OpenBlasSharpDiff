using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DTGSYL solves the generalized Sylvester equation:
        /// </para>
        /// <para>
        ///             A * R - L * B = scale * C                 (1)
        ///             D * R - L * E = scale * F
        /// </para>
        /// <para>
        /// where R and L are unknown m-by-n matrices, (A, D), (B, E) and
        /// (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
        /// respectively, with real entries. (A, D) and (B, E) must be in
        /// generalized (real) Schur canonical form, i.e. A, B are upper quasi
        /// triangular and D, E are upper triangular.
        /// </para>
        /// <para>
        /// The solution (R, L) overwrites (C, F). 0 &lt;= SCALE &lt;= 1 is an output
        /// scaling factor chosen to avoid overflow.
        /// </para>
        /// <para>
        /// In matrix notation (1) is equivalent to solve  Zx = scale b, where
        /// Z is defined as
        /// </para>
        /// <para>
        ///            Z = [ kron(In, A)  -kron(B**T, Im) ]         (2)
        ///                [ kron(In, D)  -kron(E**T, Im) ].
        /// </para>
        /// <para>
        /// Here Ik is the identity matrix of size k and X**T is the transpose of
        /// X. kron(X, Y) is the Kronecker product between the matrices X and Y.
        /// </para>
        /// <para>
        /// If TRANS = &#39;T&#39;, DTGSYL solves the transposed system Z**T*y = scale*b,
        /// which is equivalent to solve for R and L in
        /// </para>
        /// <para>
        ///             A**T * R + D**T * L = scale * C           (3)
        ///             R * B**T + L * E**T = scale * -F
        /// </para>
        /// <para>
        /// This case (TRANS = &#39;T&#39;) is used to compute an one-norm-based estimate
        /// of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
        /// and (B,E), using DLACON.
        /// </para>
        /// <para>
        /// If IJOB &gt;= 1, DTGSYL computes a Frobenius norm-based estimate
        /// of Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
        /// reciprocal of the smallest singular value of Z. See [1-2] for more
        /// information.
        /// </para>
        /// <para>
        /// This is a level 3 BLAS algorithm.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// = &#39;N&#39;: solve the generalized Sylvester equation (1).
        /// = &#39;T&#39;: solve the &#39;transposed&#39; system (3).
        /// </param>
        /// <param name="ijob">
        /// [in] IJOB is INTEGER.
        /// Specifies what kind of functionality to be performed.
        /// = 0: solve (1) only.
        /// = 1: The functionality of 0 and 3.
        /// = 2: The functionality of 0 and 4.
        /// = 3: Only an estimate of Dif[(A,D), (B,E)] is computed.
        /// (look ahead strategy IJOB  = 1 is used).
        /// = 4: Only an estimate of Dif[(A,D), (B,E)] is computed.
        /// ( DGECON on sub-systems is used ).
        /// Not referenced if TRANS = &#39;T&#39;.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The order of the matrices A and D, and the row dimension of
        /// the matrices C, F, R and L.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices B and E, and the column dimension
        /// of the matrices C, F, R and L.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension (LDA, M).
        /// The upper quasi triangular matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1, M).
        /// </param>
        /// <param name="b">
        /// [in] B is DOUBLE PRECISION array, dimension (LDB, N).
        /// The upper quasi triangular matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1, N).
        /// </param>
        /// <param name="c">
        /// [in,out] C is DOUBLE PRECISION array, dimension (LDC, N).
        /// On entry, C contains the right-hand-side of the first matrix
        /// equation in (1) or (3).
        /// On exit, if IJOB = 0, 1 or 2, C has been overwritten by
        /// the solution R. If IJOB = 3 or 4 and TRANS = &#39;N&#39;, C holds R,
        /// the solution achieved during the computation of the
        /// Dif-estimate.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C. LDC &gt;= max(1, M).
        /// </param>
        /// <param name="d">
        /// [in] D is DOUBLE PRECISION array, dimension (LDD, M).
        /// The upper triangular matrix D.
        /// </param>
        /// <param name="ldd">
        /// [in] LDD is INTEGER.
        /// The leading dimension of the array D. LDD &gt;= max(1, M).
        /// </param>
        /// <param name="e">
        /// [in] E is DOUBLE PRECISION array, dimension (LDE, N).
        /// The upper triangular matrix E.
        /// </param>
        /// <param name="lde">
        /// [in] LDE is INTEGER.
        /// The leading dimension of the array E. LDE &gt;= max(1, N).
        /// </param>
        /// <param name="f">
        /// [in,out] F is DOUBLE PRECISION array, dimension (LDF, N).
        /// On entry, F contains the right-hand-side of the second matrix
        /// equation in (1) or (3).
        /// On exit, if IJOB = 0, 1 or 2, F has been overwritten by
        /// the solution L. If IJOB = 3 or 4 and TRANS = &#39;N&#39;, F holds L,
        /// the solution achieved during the computation of the
        /// Dif-estimate.
        /// </param>
        /// <param name="ldf">
        /// [in] LDF is INTEGER.
        /// The leading dimension of the array F. LDF &gt;= max(1, M).
        /// </param>
        /// <param name="scale">
        /// [out] SCALE is DOUBLE PRECISION.
        /// On exit SCALE is the scaling factor in (1) or (3).
        /// If 0 &lt; SCALE &lt; 1, C and F hold the solutions R and L, resp.,
        /// to a slightly perturbed system but the input matrices A, B, D
        /// and E have not been changed. If SCALE = 0, C and F hold the
        /// solutions R and L, respectively, to the homogeneous system
        /// with C = F = 0. Normally, SCALE = 1.
        /// </param>
        /// <param name="dif">
        /// [out] DIF is DOUBLE PRECISION.
        /// On exit DIF is the reciprocal of a lower bound of the
        /// reciprocal of the Dif-function, i.e. DIF is an upper bound of
        /// Dif[(A,D), (B,E)] = sigma_min(Z), where Z as in (2).
        /// IF IJOB = 0 or TRANS = &#39;T&#39;, DIF is not touched.
        /// </param>
        /// <returns>
        /// =0: successful exit
        /// &lt;0: If INFO = -i, the i-th argument had an illegal value.
        /// &gt;0: (A, D) and (B, E) have common or close eigenvalues.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dtgsyl", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dtgsyl(
            MatrixLayout matrixLayout,
            char trans,
            int ijob,
            int m,
            int n,
            double* a,
            int lda,
            double* b,
            int ldb,
            double* c,
            int ldc,
            double* d,
            int ldd,
            double* e,
            int lde,
            double* f,
            int ldf,
            double* scale,
            double* dif);
    }
}
