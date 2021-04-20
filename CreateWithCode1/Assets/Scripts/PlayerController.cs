using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerController : MonoBehaviour
{
    // Start is called before the first frame update
    void Start()
    {
        
    }
     
    // Update is called once per frame
    void Update()
    {
        hirzontialInput = Input.GetAxis("Horizontal");
        verticalInput = Input.GetAxis("Vertical");
        transform.Translate(Vector3.forward * Time.deltaTime *  forwardSpeed * verticalInput);
        transform.Rotate(Vector3.up, Time.deltaTime * turnSpeed * hirzontialInput);
    }

    public float forwardSpeed;
    public float turnSpeed;
    public float hirzontialInput;
    public float verticalInput;
}
