using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DLASCL multiplies the M by N real matrix A by the real scalar
        /// CTO/CFROM.  This is done without over/underflow as long as the final
        /// result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
        /// A may be full, upper triangular, lower triangular, upper Hessenberg,
        /// or banded.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="type">
        /// [in] TYPE is CHARACTER*1.
        /// TYPE indices the storage type of the input matrix.
        /// = &#39;G&#39;:  A is a full matrix.
        /// = &#39;L&#39;:  A is a lower triangular matrix.
        /// = &#39;U&#39;:  A is an upper triangular matrix.
        /// = &#39;H&#39;:  A is an upper Hessenberg matrix.
        /// = &#39;B&#39;:  A is a symmetric band matrix with lower bandwidth KL
        /// and upper bandwidth KU and with the only the lower
        /// half stored.
        /// = &#39;Q&#39;:  A is a symmetric band matrix with lower bandwidth KL
        /// and upper bandwidth KU and with the only the upper
        /// half stored.
        /// = &#39;Z&#39;:  A is a band matrix with lower bandwidth KL and upper
        /// bandwidth KU. See DGBTRF for storage details.
        /// </param>
        /// <param name="kl">
        /// [in] KL is INTEGER.
        /// The lower bandwidth of A.  Referenced only if TYPE = &#39;B&#39;,
        /// &#39;Q&#39; or &#39;Z&#39;.
        /// </param>
        /// <param name="ku">
        /// [in] KU is INTEGER.
        /// The upper bandwidth of A.  Referenced only if TYPE = &#39;B&#39;,
        /// &#39;Q&#39; or &#39;Z&#39;.
        /// </param>
        /// <param name="cfrom">
        /// [in] CFROM is DOUBLE PRECISION.
        /// </param>
        /// <param name="cto">
        /// [in] CTO is DOUBLE PRECISION.
        /// 
        /// The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
        /// without over/underflow if the final result CTO*A(I,J)/CFROM
        /// can be represented without over/underflow.  CFROM must be
        /// nonzero.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// The matrix to be multiplied by CTO/CFROM.  See TYPE for the
        /// storage type.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.
        /// If TYPE = &#39;G&#39;, &#39;L&#39;, &#39;U&#39;, &#39;H&#39;, LDA &gt;= max(1,M);
        /// TYPE = &#39;B&#39;, LDA &gt;= KL+1;
        /// TYPE = &#39;Q&#39;, LDA &gt;= KU+1;
        /// TYPE = &#39;Z&#39;, LDA &gt;= 2*KL+KU+1.
        /// </param>
        /// <returns>
        /// 0  - successful exit
        /// &lt;0 - if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dlascl", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dlascl(
            MatrixLayout matrixLayout,
            char type,
            int kl,
            int ku,
            double cfrom,
            double cto,
            int m,
            int n,
            double* a,
            int lda);
    }
}
