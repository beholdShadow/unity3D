using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Rotation : MonoBehaviour
{
    public GameObject vfx;
    public Transform startObj;
    public Transform endObj;
    // Start is called before the first frame update
    public float time = 0; 
    void Start()
    {
        var startPos = startObj.position;
        GameObject obj = Instantiate(vfx, startPos, Quaternion.identity) as GameObject;

        var endPos = endObj.position;

        RotateTo(obj, endPos);
    }

    void RotateTo(GameObject obj, Vector3 destination)
    {
        var direction = destination - obj.transform.position;
        var rotation = Quaternion.LookRotation(direction);
        obj.transform.localRotation = Quaternion.Lerp(obj.transform.rotation, rotation, 1);
    }
    // Update is called once per frame
    void Update()
    {

        time += Time.deltaTime;
        if(time > 5)
        {
            Start();
            time = 0;
        }
    }
}
