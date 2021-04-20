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
        transform.position = new Vector3(Mathf.Max(leftBoundary, Mathf.Min(rightBoundary, transform.position.x)), transform.position.y, transform.position.z);
        horizontalInput = Input.GetAxis("Horizontal");
        transform.Translate(Vector3.right * horizontalInput * Time.deltaTime * speed);

        if (Input.GetKeyDown(KeyCode.Space))
        {
            Instantiate(perfab, transform.position + new Vector3(0, 1, (float)0.2), perfab.transform.rotation);
        }

    }

    public GameObject perfab;
    private float horizontalInput;
    public float speed;
    public float leftBoundary;
    public float rightBoundary;
}
