using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZUNGHR generates a complex unitary matrix Q which is defined as the
        /// product of IHI-ILO elementary reflectors of order N, as returned by
        /// ZGEHRD:
        /// </para>
        /// <para>
        /// Q = H(ilo) H(ilo+1) . . . H(ihi-1).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix Q. N &gt;= 0.
        /// </param>
        /// <param name="ilo">
        /// [in] ILO is INTEGER.
        /// </param>
        /// <param name="ihi">
        /// [in] IHI is INTEGER.
        /// 
        /// ILO and IHI must have the same values as in the previous call
        /// of ZGEHRD. Q is equal to the unit matrix except in the
        /// submatrix Q(ilo+1:ihi,ilo+1:ihi).
        /// 1 &lt;= ILO &lt;= IHI &lt;= N, if N &gt; 0; ILO=1 and IHI=0, if N=0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the vectors which define the elementary reflectors,
        /// as returned by ZGEHRD.
        /// On exit, the N-by-N unitary matrix Q.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,N).
        /// </param>
        /// <param name="tau">
        /// [in] TAU is COMPLEX*16 array, dimension (N-1).
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by ZGEHRD.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zunghr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zunghr(
            MatrixLayout matrixLayout,
            int n,
            int ilo,
            int ihi,
            Complex* a,
            int lda,
            Complex* tau);
    }
}
