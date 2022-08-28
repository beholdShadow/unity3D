// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "ASEShader/10_CelShading"
{
	Properties
	{
		_Albedo("Albedo", 2D) = "white" {}
		_Normal("Normal", 2D) = "bump" {}
		_AOMap("AOMap", 2D) = "white" {}
		_ToonMap("ToonMap", 2D) = "white" {}
		_SpecMap("SpecMap", 2D) = "white" {}
		_SpecTint("SpecTint", Color) = (1,0.3711651,0.0235849,0)
		_SpecIntensity("SpecIntensity", Float) = 0
		_SpecAttenuation("SpecAttenuation", Float) = 0
		_RimTint("Rim Tint", Color) = (1,1,1,0)
		_RimPower("Rim Power", Float) = 0
		_RimOffset("Rim Offset", Range( 0 , 10)) = 0
		_OutlineColor("OutlineColor", Color) = (0,0.6394176,0.8773585,0)
		_OutlineWidth("OutlineWidth", Range( 0 , 1)) = 0
		[HideInInspector] _texcoord( "", 2D ) = "white" {}
		[HideInInspector] __dirty( "", Int ) = 1
	}

	SubShader
	{
		Tags{ }
		Cull Front
		CGPROGRAM
		#pragma target 3.0
		#pragma surface outlineSurf Outline nofog  keepalpha noshadow noambient novertexlights nolightmap nodynlightmap nodirlightmap nometa noforwardadd vertex:outlineVertexDataFunc 
		
		void outlineVertexDataFunc( inout appdata_full v, out Input o )
		{
			UNITY_INITIALIZE_OUTPUT( Input, o );
			float outlineVar = _OutlineWidth;
			v.vertex.xyz += ( v.normal * outlineVar );
		}
		inline half4 LightingOutline( SurfaceOutput s, half3 lightDir, half atten ) { return half4 ( 0,0,0, s.Alpha); }
		void outlineSurf( Input i, inout SurfaceOutput o )
		{
			o.Emission = _OutlineColor.rgb;
		}
		ENDCG
		

		Tags{ "RenderType" = "Opaque"  "Queue" = "Geometry+0" }
		Cull Back
		CGINCLUDE
		#include "UnityPBSLighting.cginc"
		#include "UnityCG.cginc"
		#include "UnityShaderVariables.cginc"
		#include "Lighting.cginc"
		#pragma target 3.0
		#ifdef UNITY_PASS_SHADOWCASTER
			#undef INTERNAL_DATA
			#undef WorldReflectionVector
			#undef WorldNormalVector
			#define INTERNAL_DATA half3 internalSurfaceTtoW0; half3 internalSurfaceTtoW1; half3 internalSurfaceTtoW2;
			#define WorldReflectionVector(data,normal) reflect (data.worldRefl, half3(dot(data.internalSurfaceTtoW0,normal), dot(data.internalSurfaceTtoW1,normal), dot(data.internalSurfaceTtoW2,normal)))
			#define WorldNormalVector(data,normal) half3(dot(data.internalSurfaceTtoW0,normal), dot(data.internalSurfaceTtoW1,normal), dot(data.internalSurfaceTtoW2,normal))
		#endif
		struct Input
		{
			float3 worldPos;
			float3 worldNormal;
			INTERNAL_DATA
			float2 uv_texcoord;
		};

		struct SurfaceOutputCustomLightingCustom
		{
			half3 Albedo;
			half3 Normal;
			half3 Emission;
			half Metallic;
			half Smoothness;
			half Occlusion;
			half Alpha;
			Input SurfInput;
			UnityGIInput GIData;
		};

		uniform float _RimOffset;
		uniform sampler2D _Normal;
		uniform float4 _Normal_ST;
		uniform float _RimPower;
		uniform float4 _RimTint;
		uniform float _SpecIntensity;
		uniform float _SpecAttenuation;
		uniform sampler2D _SpecMap;
		uniform float4 _SpecMap_ST;
		uniform float4 _SpecTint;
		uniform sampler2D _AOMap;
		uniform float4 _AOMap_ST;
		uniform sampler2D _ToonMap;
		uniform sampler2D _Albedo;
		uniform float4 _Albedo_ST;
		uniform float4 _OutlineColor;
		uniform float _OutlineWidth;

		void vertexDataFunc( inout appdata_full v, out Input o )
		{
			UNITY_INITIALIZE_OUTPUT( Input, o );
			float3 Outline110 = 0;
			v.vertex.xyz += Outline110;
			v.vertex.w = 1;
		}

		inline half4 LightingStandardCustomLighting( inout SurfaceOutputCustomLightingCustom s, half3 viewDir, UnityGI gi )
		{
			UnityGIInput data = s.GIData;
			Input i = s.SurfInput;
			half4 c = 0;
			#ifdef UNITY_PASS_FORWARDBASE
			float ase_lightAtten = data.atten;
			if( _LightColor0.a == 0)
			ase_lightAtten = 0;
			#else
			float3 ase_lightAttenRGB = gi.light.color / ( ( _LightColor0.rgb ) + 0.000001 );
			float ase_lightAtten = max( max( ase_lightAttenRGB.r, ase_lightAttenRGB.g ), ase_lightAttenRGB.b );
			#endif
			#if defined(HANDLE_SHADOWS_BLENDING_IN_GI)
			half bakedAtten = UnitySampleBakedOcclusion(data.lightmapUV.xy, data.worldPos);
			float zDist = dot(_WorldSpaceCameraPos - data.worldPos, UNITY_MATRIX_V[2].xyz);
			float fadeDist = UnityComputeShadowFadeDistance(data.worldPos, zDist);
			ase_lightAtten = UnityMixRealtimeAndBakedShadows(data.atten, bakedAtten, UnityComputeShadowFade(fadeDist));
			#endif
			float3 ase_worldPos = i.worldPos;
			float3 ase_worldViewDir = normalize( UnityWorldSpaceViewDir( ase_worldPos ) );
			float2 uv_Normal = i.uv_texcoord * _Normal_ST.xy + _Normal_ST.zw;
			float3 newWorldNormal58 = (WorldNormalVector( i , UnpackNormal( tex2D( _Normal, uv_Normal ) ) ));
			float dotResult61 = dot( ase_worldViewDir , newWorldNormal58 );
			float NdotV62 = dotResult61;
			#if defined(LIGHTMAP_ON) && UNITY_VERSION < 560 //aseld
			float3 ase_worldlightDir = 0;
			#else //aseld
			float3 ase_worldlightDir = normalize( UnityWorldSpaceLightDir( ase_worldPos ) );
			#endif //aseld
			float dotResult65 = dot( newWorldNormal58 , ase_worldlightDir );
			float NdotL66 = dotResult65;
			float4 Rim119 = ( ( pow( ( 1.0 - saturate( ( _RimOffset + NdotV62 ) ) ) , _RimPower ) * ( NdotL66 * ase_lightAtten ) ) * _RimTint );
			float dotResult132 = dot( ( ase_worldViewDir + ase_worldlightDir ) , UnpackNormal( tex2D( _Normal, uv_Normal ) ) );
			float smoothstepResult135 = smoothstep( 4.5 , 5.5 , pow( dotResult132 , _SpecAttenuation ));
			float2 uv_SpecMap = i.uv_texcoord * _SpecMap_ST.xy + _SpecMap_ST.zw;
			float4 Spec139 = saturate( ( _SpecIntensity * ( smoothstepResult135 * ( 1.0 - tex2D( _SpecMap, uv_SpecMap ) ) * _SpecTint ) * ase_lightAtten ) );
			float2 uv_AOMap = i.uv_texcoord * _AOMap_ST.xy + _AOMap_ST.zw;
			UnityGI gi101 = gi;
			float3 diffNorm101 = WorldNormalVector( i , UnpackNormal( tex2D( _Normal, uv_Normal ) ) );
			gi101 = UnityGI_Base( data, 1, diffNorm101 );
			float3 indirectDiffuse101 = gi101.indirect.diffuse + diffNorm101 * 0.0001;
			float2 temp_cast_1 = ((NdotL66*0.5 + 0.5)).xx;
			float4 Shadow87 = tex2D( _ToonMap, temp_cast_1 );
			#if defined(LIGHTMAP_ON) && ( UNITY_VERSION < 560 || ( defined(LIGHTMAP_SHADOW_MIXING) && !defined(SHADOWS_SHADOWMASK) && defined(SHADOWS_SCREEN) ) )//aselc
			float4 ase_lightColor = 0;
			#else //aselc
			float4 ase_lightColor = _LightColor0;
			#endif //aselc
			float2 uv_Albedo = i.uv_texcoord * _Albedo_ST.xy + _Albedo_ST.zw;
			float4 Light99 = ( tex2D( _AOMap, uv_AOMap ).a * ( float4( ( indirectDiffuse101 + ase_lightAtten ) , 0.0 ) * Shadow87 * ase_lightColor ) * tex2D( _Albedo, uv_Albedo ) );
			c.rgb = ( Rim119 + Spec139 + Light99 ).rgb;
			c.a = 1;
			return c;
		}

		inline void LightingStandardCustomLighting_GI( inout SurfaceOutputCustomLightingCustom s, UnityGIInput data, inout UnityGI gi )
		{
			s.GIData = data;
		}

		void surf( Input i , inout SurfaceOutputCustomLightingCustom o )
		{
			o.SurfInput = i;
			o.Normal = float3(0,0,1);
		}

		ENDCG
		CGPROGRAM
		#pragma surface surf StandardCustomLighting keepalpha fullforwardshadows exclude_path:deferred vertex:vertexDataFunc 

		ENDCG
		Pass
		{
			Name "ShadowCaster"
			Tags{ "LightMode" = "ShadowCaster" }
			ZWrite On
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#pragma target 3.0
			#pragma multi_compile_shadowcaster
			#pragma multi_compile UNITY_PASS_SHADOWCASTER
			#pragma skip_variants FOG_LINEAR FOG_EXP FOG_EXP2
			#include "HLSLSupport.cginc"
			#if ( SHADER_API_D3D11 || SHADER_API_GLCORE || SHADER_API_GLES || SHADER_API_GLES3 || SHADER_API_METAL || SHADER_API_VULKAN )
				#define CAN_SKIP_VPOS
			#endif
			#include "UnityCG.cginc"
			#include "Lighting.cginc"
			#include "UnityPBSLighting.cginc"
			struct v2f
			{
				V2F_SHADOW_CASTER;
				float2 customPack1 : TEXCOORD1;
				float4 tSpace0 : TEXCOORD2;
				float4 tSpace1 : TEXCOORD3;
				float4 tSpace2 : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};
			v2f vert( appdata_full v )
			{
				v2f o;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_INITIALIZE_OUTPUT( v2f, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				Input customInputData;
				vertexDataFunc( v, customInputData );
				float3 worldPos = mul( unity_ObjectToWorld, v.vertex ).xyz;
				half3 worldNormal = UnityObjectToWorldNormal( v.normal );
				half3 worldTangent = UnityObjectToWorldDir( v.tangent.xyz );
				half tangentSign = v.tangent.w * unity_WorldTransformParams.w;
				half3 worldBinormal = cross( worldNormal, worldTangent ) * tangentSign;
				o.tSpace0 = float4( worldTangent.x, worldBinormal.x, worldNormal.x, worldPos.x );
				o.tSpace1 = float4( worldTangent.y, worldBinormal.y, worldNormal.y, worldPos.y );
				o.tSpace2 = float4( worldTangent.z, worldBinormal.z, worldNormal.z, worldPos.z );
				o.customPack1.xy = customInputData.uv_texcoord;
				o.customPack1.xy = v.texcoord;
				TRANSFER_SHADOW_CASTER_NORMALOFFSET( o )
				return o;
			}
			half4 frag( v2f IN
			#if !defined( CAN_SKIP_VPOS )
			, UNITY_VPOS_TYPE vpos : VPOS
			#endif
			) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				Input surfIN;
				UNITY_INITIALIZE_OUTPUT( Input, surfIN );
				surfIN.uv_texcoord = IN.customPack1.xy;
				float3 worldPos = float3( IN.tSpace0.w, IN.tSpace1.w, IN.tSpace2.w );
				half3 worldViewDir = normalize( UnityWorldSpaceViewDir( worldPos ) );
				surfIN.worldPos = worldPos;
				surfIN.worldNormal = float3( IN.tSpace0.z, IN.tSpace1.z, IN.tSpace2.z );
				surfIN.internalSurfaceTtoW0 = IN.tSpace0.xyz;
				surfIN.internalSurfaceTtoW1 = IN.tSpace1.xyz;
				surfIN.internalSurfaceTtoW2 = IN.tSpace2.xyz;
				SurfaceOutputCustomLightingCustom o;
				UNITY_INITIALIZE_OUTPUT( SurfaceOutputCustomLightingCustom, o )
				surf( surfIN, o );
				#if defined( CAN_SKIP_VPOS )
				float2 vpos = IN.pos;
				#endif
				SHADOW_CASTER_FRAGMENT( IN )
			}
			ENDCG
		}
	}
	Fallback "Diffuse"
	CustomEditor "ASEMaterialInspector"
}
/*ASEBEGIN
Version=18935
662;81;1246;387;3375.009;-3321.67;1;True;False
Node;AmplifyShaderEditor.CommentaryNode;103;-3157.168,1858.015;Inherit;False;1125.957;636.4103;Comment;8;45;68;58;65;66;60;61;62;Dot;1,1,1,1;0;0
Node;AmplifyShaderEditor.SamplerNode;45;-3101.163,2061.256;Inherit;True;Property;_Normal;Normal;1;0;Create;True;0;0;0;False;0;False;-1;caec619b8be789c44b5109d576c3b8e9;caec619b8be789c44b5109d576c3b8e9;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WorldNormalVector;58;-2720.111,2069.316;Inherit;False;False;1;0;FLOAT3;0,0,1;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;60;-2735.285,1908.015;Inherit;False;World;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.DotProductOpNode;61;-2418.707,2153.825;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldSpaceLightDirHlpNode;68;-2738.821,2311.425;Inherit;False;False;1;0;FLOAT;0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.CommentaryNode;152;-3566.452,4121.481;Inherit;False;2179.344;841.1301;Comment;17;139;130;129;131;133;140;132;134;147;149;151;135;146;137;138;136;145;Spec;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;128;-3523.945,3486.717;Inherit;False;1639.985;551.6638;RimLight;14;112;114;113;115;118;116;124;125;117;126;123;127;121;119;RimLight;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;62;-2255.212,2147.593;Inherit;False;NdotV;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldSpaceLightDirHlpNode;130;-3516.452,4360.711;Inherit;False;False;1;0;FLOAT;0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.DotProductOpNode;65;-2423.607,2270.894;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;129;-3502.554,4171.481;Inherit;False;World;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;131;-3242.763,4301.911;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;133;-3432.423,4526.536;Inherit;True;Property;_TextureSample1;Texture Sample 1;1;0;Create;True;0;0;0;False;0;False;-1;caec619b8be789c44b5109d576c3b8e9;caec619b8be789c44b5109d576c3b8e9;True;0;True;bump;Auto;True;Instance;45;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;112;-3382.371,3618.989;Inherit;False;62;NdotV;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;114;-3473.945,3537.869;Inherit;False;Property;_RimOffset;Rim Offset;10;0;Create;True;0;0;0;False;0;False;0;0.47;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;66;-2255.935,2264.662;Inherit;False;NdotL;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;113;-3200.945,3548.971;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DotProductOpNode;132;-3050.744,4354.195;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;84;-3499.051,3034.647;Inherit;False;66;NdotL;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;140;-3068.718,4481.253;Inherit;False;Property;_SpecAttenuation;SpecAttenuation;7;0;Create;True;0;0;0;False;0;False;0;6.8;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;107;-3535.632,2649.699;Inherit;False;1786.333;701.8325;Comment;13;47;108;99;97;106;87;94;93;95;101;85;105;86;Light;1,1,1,1;0;0
Node;AmplifyShaderEditor.ScaleAndOffsetNode;86;-3307.684,3036.297;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0.5;False;2;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;147;-2860.433,4549.487;Inherit;True;Property;_SpecMap;SpecMap;4;0;Create;True;0;0;0;False;0;False;-1;None;2f11dd6470fe5f546978f94af176b0aa;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.PowerNode;134;-2890.455,4355.399;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;105;-3295.874,2765.302;Inherit;True;Property;_TextureSample0;Texture Sample 0;1;0;Create;True;0;0;0;False;0;False;-1;caec619b8be789c44b5109d576c3b8e9;caec619b8be789c44b5109d576c3b8e9;True;0;True;bump;Auto;True;Instance;45;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SaturateNode;115;-3058.333,3549.508;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;116;-2905.412,3549.354;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LightAttenuation;125;-2943.051,3869.546;Inherit;False;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;118;-2942.212,3660.376;Inherit;False;Property;_RimPower;Rim Power;9;0;Create;True;0;0;0;False;0;False;0;0.54;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LightAttenuation;95;-2941.116,2851.22;Inherit;False;0;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;151;-2558.223,4555.434;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SamplerNode;85;-3101.792,3008.278;Inherit;True;Property;_ToonMap;ToonMap;3;0;Create;True;0;0;0;False;0;False;-1;02d32b71bb84e524284016c1e7ba33b1;02d32b71bb84e524284016c1e7ba33b1;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;149;-2556.846,4643.763;Inherit;False;Property;_SpecTint;SpecTint;5;0;Create;True;0;0;0;False;0;False;1,0.3711651,0.0235849,0;1,0.5785769,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.IndirectDiffuseLighting;101;-2952.926,2772.479;Inherit;False;Tangent;1;0;FLOAT3;0,0,1;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SmoothstepOpNode;135;-2698.492,4358.321;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;4.5;False;2;FLOAT;5.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;124;-2939.82,3764.542;Inherit;False;66;NdotL;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;87;-2762.749,3008.621;Inherit;False;Shadow;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;106;-2699.874,2830.302;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LightAttenuation;138;-2146.937,4504.226;Inherit;False;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;146;-2339.717,4356.576;Inherit;False;3;3;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;126;-2745.963,3796.851;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;117;-2712.071,3548.659;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;137;-2145.329,4209.225;Inherit;False;Property;_SpecIntensity;SpecIntensity;6;0;Create;True;0;0;0;False;0;False;0;5.44;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LightColorNode;94;-2721.799,3130.965;Inherit;False;0;3;COLOR;0;FLOAT3;1;FLOAT;2
Node;AmplifyShaderEditor.ColorNode;123;-2537.134,3676.427;Inherit;False;Property;_RimTint;Rim Tint;8;0;Create;True;0;0;0;False;0;False;1,1,1,0;0,0.7338215,0.9811321,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;127;-2515.796,3546.673;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;136;-1931.33,4341.226;Inherit;False;3;3;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SamplerNode;47;-2437.278,2787.27;Inherit;True;Property;_AOMap;AOMap;2;0;Create;True;0;0;0;False;0;False;-1;7491fafe0b67dc9428f520ac8f569dd6;7491fafe0b67dc9428f520ac8f569dd6;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;97;-2427.044,3000.137;Inherit;False;3;3;0;FLOAT3;0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SamplerNode;93;-2467.745,3127.392;Inherit;True;Property;_Albedo;Albedo;0;0;Create;True;0;0;0;False;0;False;-1;9b10cb95af69eb14f923a0ade9e43426;9b10cb95af69eb14f923a0ade9e43426;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;109;-1745.006,2136.718;Inherit;False;857.3157;350.4067;Comment;4;110;89;91;90;Outline;1,1,1,1;0;0
Node;AmplifyShaderEditor.ColorNode;90;-1671.653,2186.718;Inherit;False;Property;_OutlineColor;OutlineColor;11;0;Create;True;0;0;0;False;0;False;0,0.6394176,0.8773585,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;108;-2111.28,2977.236;Inherit;False;3;3;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;91;-1695.006,2378.925;Inherit;False;Property;_OutlineWidth;OutlineWidth;12;0;Create;True;0;0;0;False;0;False;0;0.033;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;145;-1787.158,4340.625;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;121;-2298.948,3548.64;Inherit;False;2;2;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;99;-1956.678,2996.612;Inherit;False;Light;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.OutlineNode;89;-1380.19,2248.033;Inherit;False;0;True;None;0;0;Front;True;True;True;True;0;False;-1;3;0;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;139;-1611.109,4340.433;Inherit;False;Spec;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;119;-2107.96,3536.717;Inherit;False;Rim;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;100;-1617.535,3301.563;Inherit;False;99;Light;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;110;-1162.099,2249.788;Inherit;False;Outline;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;120;-1589.965,3112.002;Inherit;False;119;Rim;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;141;-1643.762,3216.756;Inherit;False;139;Spec;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;153;-1343.844,3190.298;Inherit;False;3;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;111;-1313.427,3323.254;Inherit;False;110;Outline;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StandardSurfaceOutputNode;83;-1063.836,2999.666;Float;False;True;-1;2;ASEMaterialInspector;0;0;CustomLighting;ASEShader/10_CelShading;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;Back;0;False;-1;0;False;-1;False;0;False;-1;0;False;-1;False;0;Opaque;0.5;True;True;0;False;Opaque;;Geometry;ForwardOnly;18;all;True;True;True;True;0;False;-1;False;0;False;-1;255;False;-1;255;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;False;2;15;10;25;False;0.5;True;0;0;False;-1;0;False;-1;0;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;True;0.05;0,0,0,0;VertexOffset;True;False;Cylindrical;False;True;Relative;0;;-1;-1;-1;-1;0;False;0;0;False;-1;-1;0;False;-1;0;0;0;False;0.1;False;-1;0;False;-1;False;15;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT;0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT;0;False;9;FLOAT;0;False;10;FLOAT;0;False;13;FLOAT3;0,0,0;False;11;FLOAT3;0,0,0;False;12;FLOAT3;0,0,0;False;14;FLOAT4;0,0,0,0;False;15;FLOAT3;0,0,0;False;0
WireConnection;58;0;45;0
WireConnection;61;0;60;0
WireConnection;61;1;58;0
WireConnection;62;0;61;0
WireConnection;65;0;58;0
WireConnection;65;1;68;0
WireConnection;131;0;129;0
WireConnection;131;1;130;0
WireConnection;66;0;65;0
WireConnection;113;0;114;0
WireConnection;113;1;112;0
WireConnection;132;0;131;0
WireConnection;132;1;133;0
WireConnection;86;0;84;0
WireConnection;134;0;132;0
WireConnection;134;1;140;0
WireConnection;115;0;113;0
WireConnection;116;0;115;0
WireConnection;151;0;147;0
WireConnection;85;1;86;0
WireConnection;101;0;105;0
WireConnection;135;0;134;0
WireConnection;87;0;85;0
WireConnection;106;0;101;0
WireConnection;106;1;95;0
WireConnection;146;0;135;0
WireConnection;146;1;151;0
WireConnection;146;2;149;0
WireConnection;126;0;124;0
WireConnection;126;1;125;0
WireConnection;117;0;116;0
WireConnection;117;1;118;0
WireConnection;127;0;117;0
WireConnection;127;1;126;0
WireConnection;136;0;137;0
WireConnection;136;1;146;0
WireConnection;136;2;138;0
WireConnection;97;0;106;0
WireConnection;97;1;87;0
WireConnection;97;2;94;0
WireConnection;108;0;47;4
WireConnection;108;1;97;0
WireConnection;108;2;93;0
WireConnection;145;0;136;0
WireConnection;121;0;127;0
WireConnection;121;1;123;0
WireConnection;99;0;108;0
WireConnection;89;0;90;0
WireConnection;89;1;91;0
WireConnection;139;0;145;0
WireConnection;119;0;121;0
WireConnection;110;0;89;0
WireConnection;153;0;120;0
WireConnection;153;1;141;0
WireConnection;153;2;100;0
WireConnection;83;13;153;0
WireConnection;83;11;111;0
ASEEND*/
//CHKSM=3DA9DCE47A8437177F9F8FAF9558909822F7F03A