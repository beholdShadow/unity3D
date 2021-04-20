using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FollowPlayer : MonoBehaviour
{
    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        Vector3 offset = new Vector3(0, 4, -8);
        transform.position = vehicle.transform.position + offset ;
    }

    public GameObject vehicle;
    private Vector3 offset;
}
