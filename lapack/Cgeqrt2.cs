using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGEQRT2 computes a QR factorization of a complex M-by-N matrix A,
        /// using the compact WY representation of Q.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= N.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the complex M-by-N matrix A.  On exit, the elements on and
        /// above the diagonal contain the N-by-N upper triangular matrix R; the
        /// elements below the diagonal are the columns of V.  See below for
        /// further details.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="t">
        /// [out] T is COMPLEX array, dimension (LDT,N).
        /// The N-by-N upper triangular factor of the block reflector.
        /// The elements on and above the diagonal contain the block
        /// reflector T; the elements below the diagonal are not used.
        /// See below for further details.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T.  LDT &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The matrix V stores the elementary reflectors H(i) in the i-th column
        ///  below the diagonal. For example, if M=5 and N=3, the matrix V is
        /// </para>
        /// <para>
        ///               V = (  1       )
        ///                   ( v1  1    )
        ///                   ( v1 v2  1 )
        ///                   ( v1 v2 v3 )
        ///                   ( v1 v2 v3 )
        /// </para>
        /// <para>
        ///  where the vi&#39;s represent the vectors which define H(i), which are returned
        ///  in the matrix A.  The 1&#39;s along the diagonal of V are not stored in A.  The
        ///  block reflector H is then given by
        /// </para>
        /// <para>
        ///               H = I - V * T * V**H
        /// </para>
        /// <para>
        ///  where V**H is the conjugate transpose of V.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgeqrt2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgeqrt2(
            MatrixLayout matrixLayout,
            int m,
            int n,
            Complex32* a,
            int lda,
            Complex32* t,
            int ldt);
    }
}
