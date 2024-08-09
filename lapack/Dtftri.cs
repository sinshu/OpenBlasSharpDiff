using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DTFTRI computes the inverse of a triangular matrix A stored in RFP
        /// format.
        /// </para>
        /// <para>
        /// This is a Level 3 BLAS version of the algorithm.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="transr">
        /// [in] TRANSR is CHARACTER*1.
        /// = &#39;N&#39;:  The Normal TRANSR of RFP A is stored;
        /// = &#39;T&#39;:  The Transpose TRANSR of RFP A is stored.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  A is upper triangular;
        /// = &#39;L&#39;:  A is lower triangular.
        /// </param>
        /// <param name="diag">
        /// [in] DIAG is CHARACTER*1.
        /// = &#39;N&#39;:  A is non-unit triangular;
        /// = &#39;U&#39;:  A is unit triangular.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (0:nt-1);.
        /// nt=N*(N+1)/2. On entry, the triangular factor of a Hermitian
        /// Positive Definite matrix A in RFP format. RFP format is
        /// described by TRANSR, UPLO, and N as follows: If TRANSR = &#39;N&#39;
        /// then RFP A is (0:N,0:k-1) when N is even; k=N/2. RFP A is
        /// (0:N-1,0:k) when N is odd; k=N/2. IF TRANSR = &#39;T&#39; then RFP is
        /// the transpose of RFP A as defined when
        /// TRANSR = &#39;N&#39;. The contents of RFP A are defined by UPLO as
        /// follows: If UPLO = &#39;U&#39; the RFP A contains the nt elements of
        /// upper packed A; If UPLO = &#39;L&#39; the RFP A contains the nt
        /// elements of lower packed A. The LDA of RFP A is (N+1)/2 when
        /// TRANSR = &#39;T&#39;. When TRANSR is &#39;N&#39; the LDA is N+1 when N is
        /// even and N is odd. See the Note below for more details.
        /// 
        /// On exit, the (triangular) inverse of the original matrix, in
        /// the same storage format.
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, A(i,i) is exactly zero.  The triangular
        /// matrix is singular and its inverse can not be computed.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  We first consider Rectangular Full Packed (RFP) Format when N is
        ///  even. We give an example where N = 6.
        /// </para>
        /// <para>
        ///      AP is Upper             AP is Lower
        /// </para>
        /// <para>
        ///   00 01 02 03 04 05       00
        ///      11 12 13 14 15       10 11
        ///         22 23 24 25       20 21 22
        ///            33 34 35       30 31 32 33
        ///               44 45       40 41 42 43 44
        ///                  55       50 51 52 53 54 55
        /// </para>
        /// <para>
        ///  Let TRANSR = &#39;N&#39;. RFP holds AP as follows:
        ///  For UPLO = &#39;U&#39; the upper trapezoid A(0:5,0:2) consists of the last
        ///  three columns of AP upper. The lower triangle A(4:6,0:2) consists of
        ///  the transpose of the first three columns of AP upper.
        ///  For UPLO = &#39;L&#39; the lower trapezoid A(1:6,0:2) consists of the first
        ///  three columns of AP lower. The upper triangle A(0:2,0:2) consists of
        ///  the transpose of the last three columns of AP lower.
        ///  This covers the case N even and TRANSR = &#39;N&#39;.
        /// </para>
        /// <para>
        ///         RFP A                   RFP A
        /// </para>
        /// <para>
        ///        03 04 05                33 43 53
        ///        13 14 15                00 44 54
        ///        23 24 25                10 11 55
        ///        33 34 35                20 21 22
        ///        00 44 45                30 31 32
        ///        01 11 55                40 41 42
        ///        02 12 22                50 51 52
        /// </para>
        /// <para>
        ///  Now let TRANSR = &#39;T&#39;. RFP A in both UPLO cases is just the
        ///  transpose of RFP A above. One therefore gets:
        /// </para>
        /// <para>
        ///           RFP A                   RFP A
        /// </para>
        /// <para>
        ///     03 13 23 33 00 01 02    33 00 10 20 30 40 50
        ///     04 14 24 34 44 11 12    43 44 11 21 31 41 51
        ///     05 15 25 35 45 55 22    53 54 55 22 32 42 52
        /// </para>
        /// <para>
        ///  We then consider Rectangular Full Packed (RFP) Format when N is
        ///  odd. We give an example where N = 5.
        /// </para>
        /// <para>
        ///     AP is Upper                 AP is Lower
        /// </para>
        /// <para>
        ///   00 01 02 03 04              00
        ///      11 12 13 14              10 11
        ///         22 23 24              20 21 22
        ///            33 34              30 31 32 33
        ///               44              40 41 42 43 44
        /// </para>
        /// <para>
        ///  Let TRANSR = &#39;N&#39;. RFP holds AP as follows:
        ///  For UPLO = &#39;U&#39; the upper trapezoid A(0:4,0:2) consists of the last
        ///  three columns of AP upper. The lower triangle A(3:4,0:1) consists of
        ///  the transpose of the first two columns of AP upper.
        ///  For UPLO = &#39;L&#39; the lower trapezoid A(0:4,0:2) consists of the first
        ///  three columns of AP lower. The upper triangle A(0:1,1:2) consists of
        ///  the transpose of the last two columns of AP lower.
        ///  This covers the case N odd and TRANSR = &#39;N&#39;.
        /// </para>
        /// <para>
        ///         RFP A                   RFP A
        /// </para>
        /// <para>
        ///        02 03 04                00 33 43
        ///        12 13 14                10 11 44
        ///        22 23 24                20 21 22
        ///        00 33 34                30 31 32
        ///        01 11 44                40 41 42
        /// </para>
        /// <para>
        ///  Now let TRANSR = &#39;T&#39;. RFP A in both UPLO cases is just the
        ///  transpose of RFP A above. One therefore gets:
        /// </para>
        /// <para>
        ///           RFP A                   RFP A
        /// </para>
        /// <para>
        ///     02 12 22 00 01             00 10 20 30 40 50
        ///     03 13 23 33 11             33 11 21 31 41 51
        ///     04 14 24 34 44             43 44 22 32 42 52
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dtftri", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dtftri(
            MatrixLayout matrixLayout,
            char transr,
            char uplo,
            char diag,
            int n,
            double* a);
    }
}
