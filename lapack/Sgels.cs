using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SGELS solves overdetermined or underdetermined real linear systems
        /// involving an M-by-N matrix A, or its transpose, using a QR or LQ
        /// factorization of A.  It is assumed that A has full rank.
        /// </para>
        /// <para>
        /// The following options are provided:
        /// </para>
        /// <para>
        /// 1. If TRANS = &#39;N&#39; and m &gt;= n:  find the least squares solution of
        ///    an overdetermined system, i.e., solve the least squares problem
        ///                 minimize || B - A*X ||.
        /// </para>
        /// <para>
        /// 2. If TRANS = &#39;N&#39; and m &lt; n:  find the minimum norm solution of
        ///    an underdetermined system A * X = B.
        /// </para>
        /// <para>
        /// 3. If TRANS = &#39;T&#39; and m &gt;= n:  find the minimum norm solution of
        ///    an underdetermined system A**T * X = B.
        /// </para>
        /// <para>
        /// 4. If TRANS = &#39;T&#39; and m &lt; n:  find the least squares solution of
        ///    an overdetermined system, i.e., solve the least squares problem
        ///                 minimize || B - A**T * X ||.
        /// </para>
        /// <para>
        /// Several right hand side vectors b and solution vectors x can be
        /// handled in a single call; they are stored as the columns of the
        /// M-by-NRHS right hand side matrix B and the N-by-NRHS solution
        /// matrix X.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// = &#39;N&#39;: the linear system involves A;
        /// = &#39;T&#39;: the linear system involves A**T.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of
        /// columns of the matrices B and X. NRHS &gt;=0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is REAL array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit,
        /// if M &gt;= N, A is overwritten by details of its QR
        /// factorization as returned by SGEQRF;
        /// if M &lt;  N, A is overwritten by details of its LQ
        /// factorization as returned by SGELQF.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="b">
        /// [in,out] B is REAL array, dimension (LDB,NRHS).
        /// On entry, the matrix B of right hand side vectors, stored
        /// columnwise; B is M-by-NRHS if TRANS = &#39;N&#39;, or N-by-NRHS
        /// if TRANS = &#39;T&#39;.
        /// On exit, if INFO = 0, B is overwritten by the solution
        /// vectors, stored columnwise:
        /// if TRANS = &#39;N&#39; and m &gt;= n, rows 1 to n of B contain the least
        /// squares solution vectors; the residual sum of squares for the
        /// solution in each column is given by the sum of squares of
        /// elements N+1 to M in that column;
        /// if TRANS = &#39;N&#39; and m &lt; n, rows 1 to N of B contain the
        /// minimum norm solution vectors;
        /// if TRANS = &#39;T&#39; and m &gt;= n, rows 1 to M of B contain the
        /// minimum norm solution vectors;
        /// if TRANS = &#39;T&#39; and m &lt; n, rows 1 to M of B contain the
        /// least squares solution vectors; the residual sum of squares
        /// for the solution in each column is given by the sum of
        /// squares of elements M+1 to N in that column.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= MAX(1,M,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO =  i, the i-th diagonal element of the
        /// triangular factor of A is zero, so that A does not have
        /// full rank; the least squares solution could not be
        /// computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sgels", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sgels(
            MatrixLayout matrixLayout,
            char trans,
            int m,
            int n,
            int nrhs,
            float* a,
            int lda,
            float* b,
            int ldb);
    }
}
