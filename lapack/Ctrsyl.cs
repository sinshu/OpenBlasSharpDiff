using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CTRSYL solves the complex Sylvester matrix equation:
        /// </para>
        /// <para>
        ///    op(A)*X + X*op(B) = scale*C or
        ///    op(A)*X - X*op(B) = scale*C,
        /// </para>
        /// <para>
        /// where op(A) = A or A**H, and A and B are both upper triangular. A is
        /// M-by-M and B is N-by-N; the right hand side C and the solution X are
        /// M-by-N; and scale is an output scale factor, set &lt;= 1 to avoid
        /// overflow in X.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="trana">
        /// [in] TRANA is CHARACTER*1.
        /// Specifies the option op(A):
        /// = &#39;N&#39;: op(A) = A    (No transpose)
        /// = &#39;C&#39;: op(A) = A**H (Conjugate transpose)
        /// </param>
        /// <param name="tranb">
        /// [in] TRANB is CHARACTER*1.
        /// Specifies the option op(B):
        /// = &#39;N&#39;: op(B) = B    (No transpose)
        /// = &#39;C&#39;: op(B) = B**H (Conjugate transpose)
        /// </param>
        /// <param name="isgn">
        /// [in] ISGN is INTEGER.
        /// Specifies the sign in the equation:
        /// = +1: solve op(A)*X + X*op(B) = scale*C
        /// = -1: solve op(A)*X - X*op(B) = scale*C
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The order of the matrix A, and the number of rows in the
        /// matrices X and C. M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix B, and the number of columns in the
        /// matrices X and C. N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX array, dimension (LDA,M).
        /// The upper triangular matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,M).
        /// </param>
        /// <param name="b">
        /// [in] B is COMPLEX array, dimension (LDB,N).
        /// The upper triangular matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,N).
        /// </param>
        /// <param name="c">
        /// [in,out] C is COMPLEX array, dimension (LDC,N).
        /// On entry, the M-by-N right hand side matrix C.
        /// On exit, C is overwritten by the solution matrix X.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C. LDC &gt;= max(1,M)
        /// </param>
        /// <param name="scale">
        /// [out] SCALE is REAL.
        /// The scale factor, scale, set &lt;= 1 to avoid overflow in X.
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// = 1: A and B have common or very close eigenvalues; perturbed
        /// values were used to solve the equation (but the matrices
        /// A and B are unchanged).
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ctrsyl", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ctrsyl(
            MatrixLayout matrixLayout,
            char trana,
            char tranb,
            int isgn,
            int m,
            int n,
            Complex32* a,
            int lda,
            Complex32* b,
            int ldb,
            Complex32* c,
            int ldc,
            float* scale);
    }
}
