// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "ASECE/JinToon"
{
	Properties
	{
		_Tex("Tex", 2D) = "white" {}
		_Ramp("Ramp", 2D) = "white" {}
		_ToonIntensity("Toon Intensity", Float) = 0.13
		_OutlineWidth("Outline Width", Float) = 0.02
		_EmissionIntensity("Emission Intensity", Float) = 0.5
		_OutlineColor("OutlineColor", Color) = (0,0,0,0)
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
		

		Tags{ "RenderType" = "Opaque"  "Queue" = "Geometry+0" "IsEmissive" = "true"  }
		Cull Back
		CGINCLUDE
		#include "UnityShaderVariables.cginc"
		#include "UnityCG.cginc"
		#include "UnityPBSLighting.cginc"
		#include "Lighting.cginc"
		#pragma target 3.0
		struct Input
		{
			float2 uv_texcoord;
			float3 worldPos;
			float3 worldNormal;
		};

		uniform sampler2D _Tex;
		uniform float4 _Tex_ST;
		uniform sampler2D _Ramp;
		uniform float _ToonIntensity;
		uniform float _EmissionIntensity;
		uniform float4 _OutlineColor;
		uniform float _OutlineWidth;

		void vertexDataFunc( inout appdata_full v, out Input o )
		{
			UNITY_INITIALIZE_OUTPUT( Input, o );
			float3 vertexToFrag22_g2 = 0;
			v.vertex.xyz += vertexToFrag22_g2;
			v.vertex.w = 1;
		}

		void surf( Input i , inout SurfaceOutputStandard o )
		{
			#if defined(LIGHTMAP_ON) && ( UNITY_VERSION < 560 || ( defined(LIGHTMAP_SHADOW_MIXING) && !defined(SHADOWS_SHADOWMASK) && defined(SHADOWS_SCREEN) ) )//aselc
			float4 ase_lightColor = 0;
			#else //aselc
			float4 ase_lightColor = _LightColor0;
			#endif //aselc
			float2 uv_Tex = i.uv_texcoord * _Tex_ST.xy + _Tex_ST.zw;
			float4 tex2DNode10_g2 = tex2D( _Tex, uv_Tex );
			float3 ase_worldPos = i.worldPos;
			#if defined(LIGHTMAP_ON) && UNITY_VERSION < 560 //aseld
			float3 ase_worldlightDir = 0;
			#else //aseld
			float3 ase_worldlightDir = normalize( UnityWorldSpaceLightDir( ase_worldPos ) );
			#endif //aseld
			float3 ase_worldNormal = i.worldNormal;
			float3 ase_normWorldNormal = normalize( ase_worldNormal );
			float dotResult3_g2 = dot( ase_worldlightDir , ase_normWorldNormal );
			float2 temp_cast_0 = (saturate( (dotResult3_g2*0.5 + 0.5) )).xx;
			o.Albedo = ( ( 0.5 * ase_lightColor ) * ( tex2DNode10_g2 * tex2D( _Ramp, temp_cast_0 ) * _ToonIntensity ) ).rgb;
			o.Emission = ( tex2DNode10_g2 * _EmissionIntensity ).rgb;
			o.Alpha = 1;
		}

		ENDCG
		CGPROGRAM
		#pragma surface surf Standard keepalpha fullforwardshadows vertex:vertexDataFunc 

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
				float3 worldPos : TEXCOORD2;
				float3 worldNormal : TEXCOORD3;
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
				o.worldNormal = worldNormal;
				o.customPack1.xy = customInputData.uv_texcoord;
				o.customPack1.xy = v.texcoord;
				o.worldPos = worldPos;
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
				float3 worldPos = IN.worldPos;
				half3 worldViewDir = normalize( UnityWorldSpaceViewDir( worldPos ) );
				surfIN.worldPos = worldPos;
				surfIN.worldNormal = IN.worldNormal;
				SurfaceOutputStandard o;
				UNITY_INITIALIZE_OUTPUT( SurfaceOutputStandard, o )
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
699;73;1220;599;1733.301;507.7785;1.600551;True;False
Node;AmplifyShaderEditor.TexturePropertyNode;4;-972.7717,-212.9151;Float;True;Property;_Ramp;Ramp;1;0;Create;True;0;0;0;False;0;False;4a1a20e176d86f64695ccaa972459e52;4a1a20e176d86f64695ccaa972459e52;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;5;-966.2971,-18.87551;Float;False;Property;_ToonIntensity;Toon Intensity;2;0;Create;True;0;0;0;False;0;False;0.13;-0.04;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;10;-755.4416,-15.91275;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;14;-767.9901,-66.82666;Inherit;False;1;0;SAMPLER2D;;False;1;SAMPLER2D;0
Node;AmplifyShaderEditor.WireNode;13;-521.59,-60.42663;Inherit;False;1;0;SAMPLER2D;;False;1;SAMPLER2D;0
Node;AmplifyShaderEditor.RangedFloatNode;7;-959.1738,58.29807;Float;False;Property;_OutlineWidth;Outline Width;3;0;Create;True;0;0;0;False;0;False;0.02;0.08;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;3;-729.5369,-206.4187;Float;True;Property;_Tex;Tex;0;0;Create;True;0;0;0;False;0;False;84508b93f15f2b64386ec07486afc7a3;84508b93f15f2b64386ec07486afc7a3;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.WireNode;11;-520.9572,-12.66633;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;6;-726.2487,6.735914;Float;False;Property;_EmissionIntensity;Emission Intensity;4;0;Create;True;0;0;0;False;0;False;0.5;1.45;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;42;-699.7083,98.22931;Inherit;False;Property;_OutlineColor;OutlineColor;5;0;Create;True;0;0;0;False;0;False;0,0,0,0;0.254717,0.254717,0.254717,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;43;-437.1502,-133.9842;Inherit;False;JinToon;-1;;2;66107c916f9290244bf93b5099918375;0;6;24;SAMPLER2D;0;False;25;SAMPLER2D;0;False;26;FLOAT;0;False;34;FLOAT;0;False;30;FLOAT;0;False;27;COLOR;0,0,0,0;False;4;COLOR;33;FLOAT;36;COLOR;0;FLOAT3;23
Node;AmplifyShaderEditor.StandardSurfaceOutputNode;66;-83.28912,-288.9642;Float;False;True;-1;2;ASEMaterialInspector;0;0;Standard;ASECE/JinToon;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;Back;0;False;-1;0;False;-1;False;0;False;-1;0;False;-1;False;0;Opaque;0.5;True;True;0;False;Opaque;;Geometry;All;18;all;True;True;True;True;0;False;-1;False;0;False;-1;255;False;-1;255;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;-1;False;2;15;10;25;False;0.5;True;0;0;False;-1;0;False;-1;0;0;False;-1;0;False;-1;0;False;-1;0;False;-1;0;False;0;0,0,0,0;VertexOffset;True;False;Cylindrical;False;True;Relative;0;;-1;-1;-1;-1;0;False;0;0;False;-1;-1;0;False;-1;0;0;0;False;0.1;False;-1;0;False;-1;False;16;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT;0;False;9;FLOAT;0;False;10;FLOAT;0;False;13;FLOAT3;0,0,0;False;11;FLOAT3;0,0,0;False;12;FLOAT3;0,0,0;False;14;FLOAT4;0,0,0,0;False;15;FLOAT3;0,0,0;False;0
WireConnection;10;0;5;0
WireConnection;14;0;4;0
WireConnection;13;0;14;0
WireConnection;11;0;10;0
WireConnection;43;24;3;0
WireConnection;43;25;13;0
WireConnection;43;26;11;0
WireConnection;43;34;6;0
WireConnection;43;30;7;0
WireConnection;43;27;42;0
WireConnection;66;0;43;0
WireConnection;66;2;43;33
WireConnection;66;11;43;23
ASEEND*/
//CHKSM=822E0DD90028D0A76800001E3F24D17EE8F1AC8E